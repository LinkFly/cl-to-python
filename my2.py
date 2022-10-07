import sys
import time
import threading
import os

eval_globals = {}

def log(msg):
    try:
        file = open('C:/last-files/my-projects/commad-of-carla/log4.txt', 'a')
        file.write('\n' + '[' + str(os.getpid()) + ']: ' + msg)
        file.flush()
        file.close()
    except Exception as e:
        sys.stderr.write('failed: ' + str(e))
        exit(-1)

log('started')

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

def getTypeSpec(val):
    try:
        lisp_type = python_to_lisp_type[type(val)]
        res = lisp_type_to_sym[lisp_type]
    except:
        res = '_'
    return res

finish = False

def process_cmd(op, cmd):
    res = ''
    try:
        if op == 'e':
            log('eval: ' + str(cmd))
            res = eval(cmd, eval_globals)
            log('res: ' + str(res))
        elif op == 'x':
            log('exec: ' + str(cmd))
            res = exec(cmd, eval_globals)
            log('exec success: ' + str(res))
        elif op == 'f':
            global finish;
            finish = True
            log('finish')
    except Exception as e:
        res = "<error: " + str(e) + ">"
        log('exception: ' + str(res))

    return getTypeSpec(res) + str(res)

def prepare_res(res):
    return 'r' + str(len(res)) + '\n' + res

while True:
    try:
        log('iter')
        op = sys.stdin.read(1)
        slen = sys.stdin.readline()
        length = int(slen)
        cmd = sys.stdin.read(length)
        log("got op, cmd: '" + op + "', '" + cmd + "'")

        res = process_cmd(op, cmd)
    except Exception as e:
        res = str(e)

    prepared_res = prepare_res(res)
    sys.stdout.write(prepared_res)
    sys.stdout.flush()

    if finish:
        break
    log('sent result: ' + prepared_res)

close(file)
