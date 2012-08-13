# MAC0451/5855 - 2012 - TÓPICOS ESPECIAIS EM DESENVOLVIMENTO PARA WEB
# Desenvolver um servidor Web

## Autor
	Pedro Paulo Vezzá Campos - 7538743

## Compilação e Execução
	O arquivo enviado é um projeto Eclipse compactado, podendo ser importado nele
	para compilação e execução.  

## Manual de Uso e Testes
	O servidor web está configurado por padrão para exibir arquivos contidos no
	diretório src/www do projeto. Isto pode ser alterado na classe WebServer 
	variável diretorioBase.
	
	Cada requisição feita ao servidor gera um cookie enviado ao cliente que
	conterá no campo "qtd_visitas": 1 caso não haja cookies armazenados no 
	cliente ou i + 1 sendo i a quantidade computada de acessos gravada no 
	cliente.  Para visualizar o valor atual acesse localhost:8080/cookie.html
	
	Foi utilizada uma implementação livre (cc-by) de um interpretador de 
	linguagem Brainfuck para simular um script interpretado pelo servidor.
	Os arquivos script a serem interpretados devem ter extensão .bfhtml.
	Para testar, acesse localhost:8080/interpretador.bfhtml
	
	Para ativar o controle de acesso em um dado diretório deve-se criar um
	arquivo .autorizados contendo uma string da forma "usuario:senha" 
	codificada em base64 por linha. Para testar, acesse localhost:8080/escondido
	e forneça as credenciais usuário: lorem e senha: ipsum 

