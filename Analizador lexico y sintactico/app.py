import ply.lex as lex
import ply.yacc as yacc
from flask import Flask, render_template, request

app = Flask(__name__)

# Definición de palabras reservadas
reserved = {
    'public': 'PUBLIC',
    'static': 'STATIC',
    'void': 'VOID',
    'int': 'INT',
    'for': 'FOR',
    'System': 'SYSTEM',
    'out': 'OUT',
    'println': 'PRINTLN'
}

tokens = [
    'PABIERTO', 'PCERRADO', 'LLAVE_ABIERTA', 'LLAVE_CERRADA',
    'OPERADOR', 'SIMBOLO', 'ID', 'NUMERO', 'CADENA',
    'MENOR_IGUAL', 'INCREMENTO', 'POSITIVO', 'DOT'
] + list(reserved.values())

# Expresiones regulares para los tokens
t_PUBLIC = r'public'
t_STATIC = r'static'
t_VOID = r'void'
t_INT = r'int'
t_FOR = r'for'
t_SYSTEM = r'System'
t_OUT = r'out'
t_PRINTLN = r'println'
t_OPERADOR = r'='
t_MENOR_IGUAL = r'<='
t_INCREMENTO = r'\+\+'
t_POSITIVO = r'\+'
t_DOT = r'\.'

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')
    return t

def t_NUMERO(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_CADENA(t):
    r'"[^"]*"'
    return t

def t_SIMBOLO(t):
    r'[.;]'
    return t

t_PABIERTO = r'\('
t_PCERRADO = r'\)'
t_LLAVE_ABIERTA = r'\{'
t_LLAVE_CERRADA = r'\}'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    if t.lexer.lineno > 3:
        t.lexer.skip(len(t.value))  

t_ignore = ' \t\r'

def t_error(t):
    error_message = f"Error léxico: Caracter no válido '{t.value[0]}' en la línea {t.lexer.lineno}"
    t.lexer.errors.append(error_message)
    t.lexer.skip(1)

lexer = lex.lex()
lexer.errors = []

# Definición del parser
def p_program(p):
    '''program : PUBLIC STATIC VOID ID PABIERTO PCERRADO LLAVE_ABIERTA statements LLAVE_CERRADA'''
    p[0] = ('program', p[4], p[8])

def p_statements(p):
    '''statements : statement statements
                  | statement
                  | empty'''
    p[0] = p[1:] if len(p) > 1 else []

def p_statement(p):
    '''statement : ID OPERADOR SIMBOLO
                 | FOR PABIERTO ID OPERADOR NUMERO PCERRADO LLAVE_ABIERTA statements LLAVE_CERRADA
                 | LLAVE_ABIERTA statements LLAVE_CERRADA
                 | ID OPERADOR NUMERO SIMBOLO
                 | SYSTEM DOT OUT DOT PRINTLN PABIERTO CADENA PCERRADO SIMBOLO 
                 | INT ID OPERADOR NUMERO SIMBOLO  
                 | ID INCREMENTO SIMBOLO 
                 | NUMERO POSITIVO NUMERO SIMBOLO  
                 | ID MENOR_IGUAL NUMERO SIMBOLO  
    '''
    if len(p) == 4:  
        p[0] = ('statement', p[1], p[2], p[3])
    elif len(p) == 10:  
        p[0] = ('for_loop', p[3], p[5], p[7])  
    elif len(p) == 7:  
        p[0] = ('print', p[5])
    elif len(p) == 5:  
        p[0] = ('declare', p[1], p[2], p[4])
    elif len(p) == 4:  
        p[0] = ('increment', p[1])
    elif len(p) == 6:  
        p[0] = ('addition', p[1], p[3])
    else:
        p[0] = ('statement', p[2])


def p_empty(p):
    '''empty : '''
    pass

def p_error(p):
    if p:
        if p.lineno > 3:
            return
        error_message = f"Error sintáctico: Token inesperado '{p.value}' en la línea {p.lineno}"
        parser.errors.append(error_message)
    else:
        parser.errors.append("Error sintáctico: Entrada incompleta o inesperada al final")

parser = yacc.yacc()


@app.route('/', methods=['GET', 'POST'])
def index():
    error_message = None
    sintactico_result = None
    result_lexema = []
    contador = {}
    success_message = None  

    if request.method == 'POST':
        expresion = request.form.get('Expresion')
        lexer.lineno = 1  
        lexer.errors = [] 
        lexer.input(expresion)

        # Proceso de tokens
        for token in lexer:
            if token.type in reserved.values():
                result_lexema.append(("RESERVADO", token.value, token.lineno))
            elif token.type == "ID":
                result_lexema.append(("IDENTIFICADOR", token.value, token.lineno))
            elif token.type == "PABIERTO":
                result_lexema.append(("PARENTESIS IZQUIERDO", token.value, token.lineno))
            elif token.type == "PCERRADO":
                result_lexema.append(("PARENTESIS DERECHO", token.value, token.lineno))
            elif token.type == "LLAVE_ABIERTA":
                result_lexema.append(("DELIMITADOR", token.value, token.lineno))
            elif token.type == "LLAVE_CERRADA":
                result_lexema.append(("DELIMITADOR", token.value, token.lineno))
            elif token.type == "OPERADOR":
                result_lexema.append(("OPERADOR", token.value, token.lineno))
            elif token.type == "SIMBOLO":
                result_lexema.append(("SIMBOLO", token.value, token.lineno))
            elif token.type == "NUMERO":
                result_lexema.append(("NUMERO", token.value, token.lineno))
            elif token.type == "CADENA":
                result_lexema.append(("CADENA", token.value, token.lineno))

        # Contador de cada tipo de token
        for tipo, palabra, numero in result_lexema:
            if tipo in contador:
                contador[tipo] += 1
            else:
                contador[tipo] = 1

        # Verificar errores léxicos
        if lexer.errors:
            error_message = "\n".join(lexer.errors)
        else:
            # Análisis sintáctico
            parser.errors = []  # Reiniciar lista de errores sintácticos
            try:
                sintactico_result = parser.parse(expresion)
            except Exception as e:
                error_message = str(e)

            # Verificar errores sintácticos
            if parser.errors:
                error_message = "\n".join(parser.errors)
            else:
                success_message = "Análisis completado sin errores."  

        return render_template('index.html', tokens=result_lexema, contador=contador,
                               expresion=expresion, error_message=error_message,
                               sintactico_result=sintactico_result,
                               success_message=success_message)  

    return render_template('index.html', tokens=None, contador=None, error_message=None, success_message=None)

if __name__ == '__main__':
    app.run(debug=True)
