#! /usr/bin/octave -qf

## MAC0300 - 2012 - MÉTODOS NUMÉROCOS DA ÁLGEBRA LINEAR
## EP2 - MÉTODOS DE PROCESSAMENTO DE IMAGENS
##
## ALUNO: PEDRO PAULO VEZZÁ CAMPOS - 7538743
##

1;

function executar(arquivo)
	antiga = imread(arquivo);
	nova = equalizar_histograma(antiga);
	imwrite(nova, "nova.jpeg", 'jpeg');
endfunction

function [imagem] = filtrar_convolucao(imagem, kernel)
	imagem = conv2(imagem, kernel, 'same');
endfunction


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
endfunction

if(nargin == 0)
	fprintf(stderr, "Forneca como argumento ao programa o arquivo a ser analisado.\n");
	fprintf(stderr, "Ex: octave %s lullaby.wav [plot [algoritmo]]\n", program_name());
	fprintf(stderr, "plot = tela | arquivo | nao; Default: nao\n");
	fprintf(stderr, "algoritmo = dft | fft; Default: fft\n");
	fprintf(stderr, "Obs: Caso seja escolhido plot = tela deve-se pressionar <Enter>\n");
	fprintf(stderr, "no terminal após cada plot para continuar a execucao do programa.\n");
	return;
elseif(nargin == 1)
	algoritmo = "fft"
	plotar = "nao"
	[tempo, resultado] = executar(argv(){1}, "nao", "fft")
elseif(nargin == 2)
	algoritmo = "fft"
	plotar = argv(){2}
	[tempo, resultado] = executar(argv(){1}, argv(){2}, "fft")
else
	algoritmo = argv(){3}
	plotar = argv(){2}
	[tempo, resultado] = executar(argv(){1}, argv(){2}, argv(){3})
endif


