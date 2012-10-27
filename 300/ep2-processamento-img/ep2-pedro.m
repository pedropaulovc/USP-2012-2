#!/usr/bin/octave -qf

## MAC0300 - 2012 - MÉTODOS NUMÉROCOS DA ÁLGEBRA LINEAR
## EP2 - MÉTODOS DE PROCESSAMENTO DE IMAGENS
##
## ALUNO: PEDRO PAULO VEZZÁ CAMPOS - 7538743

1;

function [C] = aplicar_convolucao(A, K)
    [m_A, n_A] = size(A);
    [m_K, n_K] = size(K);

    C = zeros(m_A - m_K + 1, n_A - n_K + 1);

    for i = 1 : m_K
        for j = 1 : n_K
            xpart = A(m_K-i+1 : m_A-i+1, n_K-j+1 : n_A-j+1);
            C += xpart * K(i, j);
        endfor
    endfor
    
%    C = rot90(C, 2);
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


function [imagem] = suavizar(imagem)
    nova = cast(aplicar_convolucao(imagem, [1 2 1; 2 4 2; 1 2 1]/16), "uint8");
    imagem(2 : rows(nova) + 1, 2 : columns(nova) + 1) = nova;
endfunction

function [final] = aumentar_nitidez(imagem)
    imwrite(imagem, "imagem.jpg", "jpg");
    nova = aplicar_convolucao(imagem, [-1 -1 -1; -1 8 -1; -1 -1 -1]);
    imwrite(nova, "nova.jpg", "jpg");
    nova_borda = zeros(rows(nova)+2, columns(nova)+2);
    nova_borda(2:rows(nova)+1, 2:columns(nova)+1) = nova;
    imwrite(nova_borda, "nova_borda.jpg", "jpg");
    final = nova_borda + imagem;
    imwrite(final, "final.jpg", "jpg");
endfunction

function [nova] = aumentar_nitidez2(imagem)
    nova = imagem + filter2([-1 -1 -1; -1 8 -1; -1 -1 -1], imagem, "same");
endfunction


if(nargin < 2)
	fprintf(stderr, "Forneca como argumento ao programa a imagem");
	fprintf(stderr, "e o metodo a ser aplicado na imagem.\n");
	fprintf(stderr, "Ex: octave %s -metodo imagem.jpg\n", program_name());
	fprintf(stderr, "-metodo = -contrast | blur | sharpen\n");
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

inicio_extensao = length(argv(){2});
while(argv(){2}(inicio_extensao) != '.')
    inicio_extensao -= 1;
endwhile

nome_final = strcat(argv(){2}(1:inicio_extensao - 1), "-final.jpg");

if(strcmp(argv(){1}, "-contrast"))
%    nova = equalizar_contraste(original);
elseif(strcmp(argv(){1}, "-blur"))
    nova = suavizar(original);
elseif(strcmp(argv(){1}, "-sharpen"))
    nova = aumentar_nitidez2(original);
else
    fprintf(stderr, "Metodo escolhido invalido. Encerrando.\n");
    exit();
endif
   
imwrite(nova, nome_final, "jpg");


