from decimal import *
debug = False #Controls whether or not the debug statements are printed
definitions = dict()
reserved_words = ["+", "-", "*", "/", "define", "if", "and", "or", "not", "cond", ">", "<", "=", ">=", "<=", "quote", "car", "cdr", "cons", "list", "null?", "remainder", "lambda"]
#@TODO:
#
#Math:
#Arbitrary precision arithmetic
#Rationals
#Unary division? (/ x) = 1/x
#
#
#Other features:
#Lambda functions
#Let function
#Returning functions from functions (functional replacement)
#Passing functions? this might work already
#Inline definitions (define (define)):
#Clear all the defines first, recursively, then what's left is the definition - scoping
#Variadic functions (define (f . args))
#Nil/empty list constant
#pair?, null?, append function
#Difference between ints and strings of ints:(+ 1 '2) should be 3 instead of an exception
#
#General:
#Error messages
#Arity checking
#Scoping, environments
#Clean up debug statements to actually be useful
#Stack limits: (* 8 (pi-sum 1 100-1000)) stops working eventually

def run():
#The main method that only gets invoked if evaluator.py is the calling program, for quick testing mostly
	global debug
	debug = True
	exp = """(define (square x) ((lambda (x) (* x x)) x))
	(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))(f 2 3)"""
	#print(evaluate(exp))
	highlight_references([1, 2, 3])
	while(False):
		exp = input()
		if(exp == ""):
			break
		else:
			if(exp[0] == '!'):
				if(exp[1:] == "displayfuncs"):
					display_functions()
				elif(exp[1:] == "displayvars"):
					display_variables()
			else:
				try:
					print(parse(exp))
					print(evaluate(exp))
				except Exception as e:
					print(str(e))
		
def evaluate(expression): #Might have multiple expressions here, split it up and return a list of the results
#Reads a string expression which is then parsed into a list 'tree' and evaluated
	debug_print("expression: " + str(expression))
	try:
		expr = parse(expression)
	except Exception as e:
		return "Error: " + str(e)
	debug_print("parsed: <" + str(expr)+">")
	returns = []
	for subexpr in expr:
		try:
			returns.append(eval(subexpr))
		except Exception as e:
			returns.append("Error: " + str(e))
			break
	if(is_list(returns)):
		return returns
	return returns[0]

def eval(expression):
#Reads an expression in list 'tree' form and evaluates the result
	if(is_list(expression)): #Anything that's not an atom
		return eval_function(expression)
	elif(is_string(expression)):
		return expression;
	elif(is_int(expression)):
		return int(expression)
	elif(is_float(expression)): #Floats are implemented as Decimals
		return Decimal(expression)
	elif(is_rational(expression)): #Not implemented
		return expression
	elif(is_variable(expression)): #Technically, this checks if the expression represents a previously assigned variable
		return definitions[expression][1]
	else:
		wrong("Unknown form or undeclared variable: <{0}>".format(expression))

def eval_function(expression): #Handles anything that isn't an atom - applies the function to all the arguments
	#Arity checking coming soon
	debug_print("Evaling <" + str(expression) + ">")
	function_name = expression[0]
	if(is_list(function_name)): #lambda
		return(apply_function(eval(function_name), expression[1:]))
	if(function_name in definitions.keys() and definitions[function_name][0] > 0): #User-defined functions take precedence over everything
		return(apply_function(definitions[function_name], expression[1:], function_name))
	elif(function_name == "+"):
		sum = 0
		for term in expression[1:]:
			try:
				summand = eval(term)
				if(not is_number(summand)):
					raise Exception("'{0}' is not a number".format(summand))
				sum += summand
			except Exception as e:
				wrong("Invalid operand for +: {0} in expression <{1}>: {2}".format(term, expression, str(e)))
		return sum
	elif(function_name == "-"):
		term1 = eval(expression[1])
		if(not is_number(term1)):
			wrong("Invalid operand for -: {0} in expression <{1}>".format(term1, expression))
		if(len(expression[1:]) == 1): #Unary minus
			return term1 * -1
		difference = term1
		for term in expression[2:]:
			try:
				subtrahend = eval(term)
				if(not is_number(subtrahend)):
					raise Exception("'{0}' is not a number".format(subtrahend))
				difference -= subtrahend
			except Exception as e:
				wrong("Invalid operand for -: {0} in expression <{1}>: {2}".format(term, expression, str(e)))
		return difference
	elif(function_name == "*"):
		product = 1
		for term in expression[1:]:
			try:
				multiplicand = eval(term)
				if(not is_number(multiplicand)):
					raise Exception("'{0}' is not a number".format(multiplicand))
				product *= multiplicand
			except Exception as e:
				wrong("Invalid operand for *: {0} in expression <{1}>: {2}".format(term, expression, str(e)))
		return product
	elif(function_name == "/"):
		#Unary divide here, once rationals are implemented
		quotient = eval(expression[1])
		if(not is_number(quotient)):
			wrong("Invalid operand for /: {0} in expression <{1}>".format(quotient, expression))
		if(len(expression[1:]) == 1):
			return 1 / quotient
		for term in expression[2:]:
			try:
				divisor = eval(term)
				if(not is_number(divisor)):
					raise Exception("'{0}' is not a number".format(divisor))
				elif(divisor == 0):
					raise Exception("Cannot divide by zero")
				quotient /= divisor
			except Exception as e:
				wrong("Invalid operand for /: {0} in expression <{1}>: {2}".format(term, expression, str(e)))
		return quotient
	elif(function_name == "define"): #Function and variable definition here
		signature = expression[1] #Signature: [name, arg1, arg2, ...]
		body = expression[2] #Actual definition
		if(is_list(signature)): #If signature has 2+ arguments, this is defining a function
			definitions[signature[0]] = escape_args(signature[1:], body)
			#Define the function to be the body, with the variables replaced by generics: see escape_args()
			debug_print(definitions[signature[0]])
		else: #This is defining a variable, so just set the value of that variable
			value = eval(body)
			if(is_list(body) and body[0] == "lambda"): #lambda
				definitions[signature] = value
			else:
				definitions[signature] = [0, value]
			debug_print("<{0}: {1}>".format(signature, definitions[signature]))
	elif(function_name == "if"):
		predicate = eval(expression[1])
		consequent = expression[2]
		alternative = expression[3]
		if(predicate == "#t"):
			return eval(consequent)
		elif(predicate == "#f"):
			return eval(alternative)
		else:
			wrong("Improper form in predicate of 'if': <{0}>".format(predicate))
	elif(function_name == "and"):
		for term in expression[1:]:
			predicate = eval(term)
			if(predicate == "#f"):
				return "#f"
			elif(not predicate == "#t"):
				wrong("Improper value in predicate of 'and': <{0}>".format(predicate))
		return "#t"
	elif(function_name == "or"):
		for term in expression[1:]:
			predicate = eval(term)
			if(predicate == "#t"):
				return "#t"
			elif(not predicate == "#f"):
				wrong("Improper value in predicate of 'and': <{0}>".format(predicate))
		return "#f"
	elif(function_name == "not"):
		predicate = eval(expression[1])
		if(predicate == "#t"):
			return "#f"
		elif(predicate == "#f"):
			return "#t"
		else:
			wrong("Improper value in predicate of 'not': <{0}>".format(predicate))
	elif(function_name == "cond"):
		for pair in expression[1:]:
			term = pair[0]
			consequent = pair[1:]
			if(term == "else"):
				if(len(pair) == 2):
					return eval(consequent[0])
				value = "";
				for cons in consequent:
					value = eval(cons)
				return value
			else:
				predicate = eval(term)
				if(predicate == "#t"):
					if(len(pair) == 2):
						return eval(consequent[0])
					value = "";
					for cons in consequent:
						value = eval(cons)
					return value
				elif(not predicate == "#f"):
					wrong("Improper value in predicate of 'cond': <{0}>".format(predicate))
		wrong("No true predicate in 'cond' and no else statement: <{0}>".format(expression))
	elif(function_name == ">"): #Check for numbers
		if(eval(expression[1]) > eval(expression[2])):
			return "#t"
		return "#f"
	elif(function_name == "<"):
		if(eval(expression[1]) < eval(expression[2])):
			return "#t"
		return "#f"
	elif(function_name == "="):
		if(eval(expression[1]) == eval(expression[2])):
			return "#t"
		return "#f"
	elif(function_name == "<="):
		if(eval(expression[1]) <= eval(expression[2])):
			return "#t"
		return "#f"
	elif(function_name == ">="):
		if(eval(expression[1]) >= eval(expression[2])):
			return "#t"
		return "#f"
	elif(function_name == "quote"):
		return expression[1]
	elif(function_name == "car"):
		return car(expression[1])
	elif(function_name == "cdr"):
		return cdr(expression[1])
	elif(function_name == "cons"):
		pair = [eval(expression[1])]
		pair.append(eval(expression[2]))
		return pair
	elif(function_name == "list"):
		return expression[1:]
	elif(function_name == "null?"):
		if(eval(expression[1]) == []):
			return "#t"
		else:
			return "#f"
	elif(function_name == "remainder"):
		return eval(expression[1]) % eval(expression[2])
	elif(function_name == "lambda"):
		debug_print(str(expression))
		return escape_args(expression[1], expression[2])
	else:
		wrong("<{0}> is not a valid operator in expression <{1}>.".format(expression[0], expression))
		return None


def apply_function(fun_body, args, function_name = ""): #Applies the function to the arguments
	body = fun_body[1]
	required_args = fun_body[0]
	if(function_name == ""):
		function_name = body[1]
	debug_print("Applying <{0}> to <{1}>".format(body, args))
	given_args = len(args)
	if(required_args != given_args):
		wrong("Incorrect number of arguments for function <{0}>: The function takes {1} argument{2} but {3} were given".format(function_name, required_args, "s"*(1 - (int)(1/required_args)), given_args))
	body = unescape_args(args, fun_body)
	debug_print(body)
	#Here, we substitute (expand) the body of the function with the given arguments, instead of evaluating them as they come up
	#Remains to be seen if this is going to cause some performance problem or bugs, so watch this
	return(eval(body))

def escape_args(args, body):
	#Replace the temporary variables with index variables: For instance, (define (square x) (* x x)) is mapped to [*, {0}, {0}]
	#And (define (sum-of_squares x y) (+ (square x) (square y))) becomes [+ [square, {0}] [square, {1}]]
	#The order of the variables is kept, but the names are anonymized
	if(not is_list(body)):
		for args_index in range(len(args)):
				if(body == args[args_index]):
					body = "{" + str(args_index) + "}"
		return [len(args), body]
	for body_index in range(len(body)):
		if(is_list(body[body_index])):
			body[body_index] = escape_args(args, body[body_index])
		else:
			for args_index in range(len(args)):
				if(body[body_index] == args[args_index]):
					body[body_index] = "{" + str(args_index) + "}"
	return [len(args), body]

def unescape_args(args, fun_body):
	#This is the opposite of escape_args - actual arguments are substituted into the body to replace the anonymous ones
	fun_body = fun_body[1]
	if(not is_list(fun_body)):
		if(fun_body[0] == '{' and fun_body[-1] == '}'):
			arg_index = int(fun_body[1:-1])
			body = args[arg_index]
		else:
			body = fun_body
		debug_print("Unescaping: " + str(body))
		return body
	body = list(fun_body)
	for body_index in range(len(body)):
		if(is_list(body[body_index])):
			body[body_index] = unescape_args(args, body[body_index])
		else:
			if(body[body_index][0] == '{' and body[body_index][-1] == '}'):
				arg_index = int(body[body_index][1:-1])
				body[body_index] = args[arg_index]
	debug_print("Unescaping: " + str(body))
	return body

def unquote(expression):
	quote_index = expression.find("'")
	if(quote_index < 0):
		return expression
	new_expression = expression[:quote_index]
	rest = expression[quote_index+1:]
	new_expression += "(quote "
	if(rest[0] == '('):
		num_parens = 0
		index = 0
		while(index < len(rest)):
			char = rest[index]
			new_expression += char
			if(char == '('):
				num_parens += 1
			elif(char == ')'):
				num_parens -= 1
				if(num_parens == 0):
					new_expression += ")" + rest[index+1:]
					if(new_expression.find("'") < 0):
						return new_expression
					else:
						return unquote(new_expression)
			index += 1
	else:
		index = 0
		while(index < len(rest)):
			char = rest[index]
			if(char not in ['(', ' ', ')']):
				new_expression += char
			else:
				new_expression += ")" + rest[index:]
				if(new_expression.find("'") < 0):
					return new_expression
				else:
					return unquote(new_expression)
			index += 1
	debug_print("Something went wrong")
	return expression

def parse(expression):
	expression = expression.strip()
	expression = unquote(expression)
	#Converts a string expression to a list 'expression tree'
	expr = []
	buffer = ""
	char = ''
	index = 0
	numParens = 0
	while(index < len(expression)):
		char = expression[index]
		buffer += char
		if((char == ' ' or char == '\t' or char == '\n' or char == '\r') and numParens == 0):
			#Whitespace\newline\tab is a delimiter, but only one counts
			if(len(buffer) > 1):
				expr.append(buffer[:-1])
			buffer = ""
		elif(char == '('):
			if(len(buffer) > 1 and numParens == 0):
				expr.append(buffer[:-1])
				buffer = ""
			numParens += 1
		elif(char == ')'):
			#Each parentheses block is a subexpression, so create a new list for it
			numParens -= 1
			if(numParens == 0):
				if(buffer[0] == '('):
					expr.append(parse(buffer[1:-1]))
				else:
					expr.append(parse(buffer[:-1]))
				buffer= ""
		index += 1
	if(len(buffer) > 0):
		expr.append(buffer)
		#Add any trailing items
	if(numParens != 0):
		wrong("Mismatched parentheses in <{0}>".format(expression))
	return expr

def car(list):
	list = eval(list)
	if(is_list(list)):
		return list[0]
	debug_print("car: " + str(list) + ", " + str(list[0]))
	return []

def cdr(list):
	list = eval(list)
	if(is_list(list)):
		if(len(list) > 0):
			return list[1:]
		else:
			return []
	else:
		debug_print("Error: <{0}> is not a list.".format(list))
		return []
		
def is_list(expression):
	try:
		if(not isinstance(expression, str)):
			return(len(expression) > 1)
		return False
	except:
		return False

def is_number(value):
	return is_int(value) or is_float(value) or is_rational(value)

def is_float(value):
	try:
		float(value)
		return True
	except:
		return False

def is_string(value):
	if(not is_list(value)):
		if(value[0] == '"' and value[-1] == '"'):
			return True
		
def is_int(value):
	try:
		int(value)
		return True
	except:
		return False

def is_rational(value): #Non-functional
	try:
		num_den = value.split("/")
		if(len(num_den) != 2):
			return False
		return(is_int(num_den[0]) and is_int(num_den[1]))
	except:
		return False

def is_variable(expression):
	return(expression in definitions.keys() and definitions[expression][0] == 0)

def is_quoted(expression): #Non-functional
	try:
		return (expression[0] == "'" or expression.find("quote") == 0)
	except:
		return False
		
def debug_print(str):
	if(debug):
		print(str)

def wrong(str):
	raise Exception(str)

def display_variables():
	print(str(definitions))

def display_functions():
	print(str(definitions))
	
def upscope():
	global scope
	if(scope == ""):
		wrong("Trying to return scope at highest level")
	index = scope[:-1].rfind("/")
	scope = scope[0:max(index+1, 0)]

tokens, scopetokens = [], []
scope = ""
count = 0
def reference_highlight(data):
	global tokens
	text = data[0]
	exp = parse(text)
	tokenize_linear(exp)
	addscopes(exp)
	start_index = data[2]
	instances = data[4]
	reference_name = data[1]
	reference_index = -1
	for i in range(len(instances)):
		if(instances[i][0] == start_index):
			reference_index = i
			break
	if(reference_index == -1):
		print("Error, something went wrong with " + str(data))
	print("Reference name: {0}, reference index: {1}".format(reference_name, reference_index))
	tokenindex = -1
	count = -1
	for i in range(len(tokens)):
		tokenindex += 1
		if(tokens[i] == reference_name):
			count += 1
			if(reference_index == count):
				break
	scope_name = scopetokens[tokenindex]
	firstflag = True
	count = 0
	for i in range(len(tokens)):
		if(tokens[i] == reference_name and scopetokens[i] == scope_name):
			if(firstflag):
				instances[count][2] = 2
				firstflag = False
			else:
				instances[count][2] = 1
			count += 1
	return (data[0], data[1], data[2], data[3], instances)
   
def addscopes(exp):
	global scope, count, scopetokens
	if(not isinstance(exp, str) and len(exp) > 0):
		if(exp[0] == "define" or exp[0] == "lambda"):
			scope += "arg" + str(count) + "/"
			count += 1
			for e in exp:
				addscopes(e)
			upscope()
		else:
			for e in exp:
				addscopes(e)
	else:
		scopetokens.append(scope + exp)

def tokenize_linear(exp):
	if(not isinstance(exp, str) and len(exp) > 0):
		for e in exp:
			tokenize_linear(e)
	else:
		tokens.append(exp)
   
   
if(__name__ == "__main__"):
	run()
