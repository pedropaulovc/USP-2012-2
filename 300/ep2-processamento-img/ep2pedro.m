#!/usr/bin/octave -qf

## MAC0300 - 2012 - MÉTODOS NUMÉROCOS DA ÁLGEBRA LINEAR
## EP2 - MÉTODOS DE PROCESSAMENTO DE IMAGENS
##
## ALUNO: PEDRO PAULO VEZZÁ CAMPOS - 7538743

1;

#Responsável por aplicar uma filtragem por convolucao em uma imagem.
#Equivalente a filter2(K, A, "same")
#Retorno:
# C = A imagem filtrada com o kernel K, de mesmo tamanho de A
#Parâmetros:
# - A: A imagem original a ser filtrada
# - K: O kernel a ser aplicado na filtragem
function [C] = aplicar_convolucao(A, K)
    [m_A, n_A] = size(A);
    [m_K, n_K] = size(K);
	
	borda = zeros(m_A + 2, n_A + 2);
	borda(2 : m_A + 1, 2: n_A + 1) = A;
	A = borda;
	m_A += 2;
	n_A += 2;
	
    C = zeros(m_A - m_K + 1, n_A - n_K + 1);

    for i = 1 : m_A - 2
        for j = 1 : n_A - 2
            C(i, j) = sum(sum(A(i : i + m_K - 1, j : j + m_K - 1) .* K));
        endfor
    endfor
endfunction

#Responsável por aumentar o contraste de uma imagem atraves da equalizacao
#de histograma
#Retorno:
# imagem = A imagem filtrada com histograma equalizado
#Parâmetros:
# - imagem: A imagem original a ser filtrada
function [imagem] = equalizar_histograma(imagem)
	m = rows(imagem);
	n = columns(imagem);
	p = zeros(256, 1);
	fda = zeros(256, 1);
	
	for j = 1 : n
		for i = 1 : m
			p(imagem(i,j) + 1) += 1;
		endfor
	endfor
	
	fda(1) = p(1);
	for i = 2 : 256
		fda(i) = fda(i - 1) + p(i);
	endfor
	
	fda_min = min(fda);
	fda_max = max(fda);
	
	h = zeros(256,1);
	for i = 1 : 256
		h(i) = round((fda(i) - fda_min)/(m * n - fda_min) * 255);
	endfor
	
	for j = 1 : n
		for i = 1 : m
			imagem(i, j) = h(imagem(i,j) + 1);
		endfor
	endfor

	hist_n = zeros(256, 1);
	for j = 1 : n
		for i = 1 : m
			hist_n(imagem(i,j) + 1) += 1;
		endfor
	endfor
	
%	imprimir_histogramas(p, hist_n)	
endfunction

#Funcao auxiliar para impressao de histogramas das imagens antes e depois da 
#equalizacao do histograma da imagem
#Retorno:
# nenhum. Grava no diretorio corrente duas imagens: hist_original.jpg e 
#hist_novo.jpg contendo os histogramas antes e depois da filtragem
#Parâmetros:
# - imagem: O histograma da imagem original
# - novo: O histograma da imagem nova
function imprimir_histogramas(original, novo)
	figure ("visible", "off"); 
	
	title('Histograma original');
	xlabel('Intensidade');
	ylabel('Contagem');
	plot([0:255], original);
	print('hist_original.jpg');

	title('Histograma novo');
	xlabel('Intensidade');
	ylabel('Contagem');
	plot([0:255], novo);
	print('hist_novo.jpg');
endfunction

#Responsável por aplicar uma filtragem de suavizacao da imagem atraves da 
#aplicacao de uma filtragem por convolucao com kernel = [1 2 1; 2 4 2; 1 2 1]/16
#Retorno:
# final = A imagem suavizada
#Parâmetros:
# - imagem: A imagem original a ser filtrada
function [final] = suavizar(imagem)
    final = cast(aplicar_convolucao(imagem, [1 2 1; 2 4 2; 1 2 1]/16), "uint8");
endfunction

#Responsável por aplicar uma filtragem de aumento de nitidez da imagem atraves da 
#aplicacao de um operador Laplaciano implementado por uma filtragem por
#convolucao com kernel = [-1 -1 -1; -1 8 -1; -1 -1 -1]
#Retorno:
# final = A imagem suavizada
# convolucao = A imagem intermediaria da aplicacao do operador Laplaciano
#Parâmetros:
# - imagem: A imagem original a ser filtrada
function [final, convolucao] = aumentar_nitidez(imagem)
	convolucao = aplicar_convolucao(imagem, [-1 -1 -1; -1 8 -1; -1 -1 -1]);
    final = imagem + convolucao;
%    imwrite(convolucao, "convolucao.jpg", "jpg");
endfunction

if(nargin < 2)
	fprintf(stderr, "Forneca como argumento ao programa a imagem");
	fprintf(stderr, "e o metodo a ser aplicado na imagem.\n");
	fprintf(stderr, "Ex: octave %s -metodo imagem.jpg\n", program_name());
	fprintf(stderr, "-metodo = -contrast | -blur | -sharpen\n");
    fprintf(stderr, "O programa salva, na pasta de execução do programa,");
	fprintf(stderr, "o arquivo da imagem processada em formato jpg, de,");
	fprintf(stderr, "nome igual ao arquivo de entrada, seguido do sufixo");
	fprintf(stderr, "final. Exemplo: imagem-final.jpg\n");
	return;
endif

try
	original = imread(argv(){2});
catch 
	fprintf(stderr, "Não foi possível abrir o arquivo fornecido. Encerrando.\n");
	exit();
end_try_catch

inicio_nome = length(argv(){2});
while(inicio_nome >= 1 && argv(){2}(inicio_nome) != '/')
    inicio_nome -= 1;
endwhile

inicio_extensao = length(argv(){2});
while(argv(){2}(inicio_extensao) != '.')
    inicio_extensao -= 1;
endwhile

nome_final = strcat(argv(){2}(inicio_nome + 1:inicio_extensao - 1), "-final.jpg");

if(strcmp(argv(){1}, "-contrast"))
    nova = equalizar_histograma(original);
elseif(strcmp(argv(){1}, "-blur"))
    nova = suavizar(original);
elseif(strcmp(argv(){1}, "-sharpen"))
	nova = aumentar_nitidez(original);
else
    fprintf(stderr, "Metodo escolhido invalido. Encerrando.\n");
    exit();
endif
   
imwrite(nova, nome_final, "jpg");


