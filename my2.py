import sys
import time
import threading
import os
import psutil

### Config vars
log_file_name = "log.txt"
log_dir_name = 'logs'

### Evaluating vars
this_dir = os.path.dirname(os.path.abspath(__file__))
# file for logging
log_path = this_dir + '/' + log_dir_name + '/' + log_file_name

### Internal vars
# Evaluating context
eval_globals = {}
# Finish main loop
finish = False
# Control of running thread for checking parent (when parent exit - exit this process)
isRunCheckParent = True
# Control main loop - set False for finish process
bMainRun = True

# Mapping data
python_to_lisp_type = {
	bool: "BOOLEAN",
	type(None): "NULL",
	int: "INTEGER",
	float: "FLOAT",
	complex: "COMPLEX",
	list: "VECTOR",
	dict: "HASH-TABLE",
	str: "STRING",
}

### Type prefixes ###
lisp_type_to_sym = {
    "BOOLEAN": 'b',
    "NULL": 'n',
    "INTEGER": 'i',
    "FLOAT": 'f',
    "COMPLEX": 'c',
    "VECTOR": 'v',
    "HASH-TABLE": 'h',
    "STRING": 's',
}
sym_pass = 'p'
sym_error = 'e'
### end Type prefixes ###

def log(msg):
    global file_log
    try:
        # TODO filename to variable
        file_log = open(log_path, 'a')
        file_log.write('\n' + '[' + str(os.getpid()) + ']: ' + msg)
        file_log.flush()
        file_log.close()
    except Exception as e:
        sys.stderr.write('failed: ' + str(e))
        exit(-1)


def get_pass_type_spec():
    return sym_pass


def get_error_type_spec(val):
    return sym_error


def get_type_spec(val):
    
    try:
        lisp_type = python_to_lisp_type[type(val)]
        local_res = lisp_type_to_sym[lisp_type]
    except Exception as err:
        local_res = get_error_type_spec(err) + '<error get_type_spec(%s)>' % str(err)
    return local_res


def thread_runner(cmd):
    return eval(cmd, eval_globals)


def process_cmd(operation, cmd):
    local_res = ''
    try:
        if operation == 'e':
            log('eval: ' + str(cmd))
            local_res = eval(cmd, eval_globals)
            log('local_res: ' + str(local_res))
        elif operation == 'x':
            log('exec: ' + str(cmd))
            local_res = exec(cmd, eval_globals)
            log('exec success: ' + str(res))
        elif operation == 'p':
            log('parallel eval: ' + str(cmd))
            th = threading.Thread(target=thread_runner, args=(cmd,))
            logging.info("Before running thread for eval")
            th.start()
            logging.info("After running thread for eval")
            local_res = None
            log('process_cmd local_res: ' + str(local_res))
        elif operation == 'f':
            global finish
            finish = True
            log('finish (in process_cmd)')
    except Exception as err:
        local_res = err
        log('exception (into process_cmd): ' + str(local_res))

    return local_res


def prepare_res(res, uuid):
    local_uuid = uuid
    if uuid == '':
        # Note: uuid length must be = 38
        local_uuid = '<unknown_uuid_----------------------->'
    payload = local_uuid + res
    return 'r' + str(len(payload)) + '\n' + payload


def check_pid(pid):
    """ Check For the existence of a unix pid. """
    return psutil.pid_exists(pid)


def thread_func_check_process_parent():
    global isRunCheckParent, bMainRun
    log('=== thread_func_check_process_parent: started')
    try:
        while isRunCheckParent:
            #log('iter thread_func_check_process_parent')
            ppid = os.getppid()
            #log('parent pid = ' + str(ppid))
            bParentAlive = check_pid(ppid)
            #log('parent is alive = ' + str(bParentAlive))
            if bParentAlive:
                #log('next-iter')
                time.sleep(1)
            else:
                log('thread_func_check_process_parent: exit')
                bMainRun = False
                isRunCheckParent = False
    except Exception as e:
        log('Error! Exception: ' + str(e))
        sys.exit(1)
    log('=== thread_func_check_process_parent: finish')

log('started')

# Run cleanup threads (kill this process when parent process exited
thCheckParent = threading.Thread(target=thread_func_check_process_parent, args=())
thCheckParent.start()

data = ''
restData = ''
stage_wait_op = 1
stage_wait_len = 2
stage_wait_cmd = 3
stage_process_cmd = 3
cur_stage = stage_wait_op
uuid = ''
cmd = ''
res = ''

# TODO It to function
while True:
    try:
        #log('iter')
        lastData = sys.stdin.read(1)

        # Maybe don't required
        if len(lastData) == 0:
            continue

        data = restData + lastData

        ### Reading commands ###
        # Read op
        if cur_stage == stage_wait_op:
            op = data[0]
            if op == 'f':
                log('finish')
                break
            cur_stage = stage_wait_len
        # Read length
        if cur_stage == stage_wait_len:
            nl_pos = data.find('\n')
            if nl_pos == -1:
                restData = data
                continue
            else:
                cmd_len = int(data[1:nl_pos])
                cur_stage = stage_wait_cmd
        # Read payload
        if cur_stage == stage_wait_cmd:
            if len(data) < nl_pos + len('\n') + cmd_len:
                restData = data
                continue
            posCmd = nl_pos + 1
            posEndCmd = posCmd + cmd_len
            cmd = data[posCmd:posEndCmd]
            restData = data[posEndCmd:]
            cur_stage = stage_process_cmd
        # Processing payload
        if cur_stage == stage_process_cmd:
            # Cut special string sequences
            #cmd = cmd.replace('e0\n', '')
            #cmd.replace("ee00\\\\nn", "e0\\n")
            log("got op, cmd: '" + op + "', '" + cmd + "'")
            # TODO 38 to variable
            uuid = cmd[0:38]
            cmd = cmd[38:]
            if cmd == '':
                res = get_pass_type_spec()
                log('pass')
            else:
                res = process_cmd(op, cmd)
                res = get_type_spec(res) + str(res)
    except Exception as e:
        res = '<error_' + str(e) + 'op = %s, uuid = %s, cmd = %s' % (op, uuid, cmd) + '>'
        res = get_error_type_spec(res) + res

    if res != get_pass_type_spec():
        prepared_res = prepare_res(res, uuid)
        sys.stdout.write(prepared_res)
        sys.stdout.flush()
        log('sent result: ' + prepared_res)
    uuid = ''
    cmd = ''
    res = ''
    cur_stage = stage_wait_op

#time.sleep(1)
