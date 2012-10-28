#!/usr/bin/octave -qf

## MAC0300 - 2012 - MÉTODOS NUMÉROCOS DA ÁLGEBRA LINEAR
## EP2 - MÉTODOS DE PROCESSAMENTO DE IMAGENS
##
## ALUNO: PEDRO PAULO VEZZÁ CAMPOS - 7538743

1;

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


function [final] = suavizar(imagem)
    final = cast(aplicar_convolucao(imagem, [1 2 1; 2 4 2; 1 2 1]/16), "uint8");
endfunction

function [final] = suavizar2(imagem)
    final = cast(filter2([1 2 1; 2 4 2; 1 2 1]/16, imagem, "same"), "uint8");
endfunction

function [final] = aumentar_nitidez(imagem)
    final = imagem + aplicar_convolucao(imagem, [-1 -1 -1; -1 8 -1; -1 -1 -1]);
endfunction

function [final] = aumentar_nitidez2(imagem)
    final = imagem + filter2([-1 -1 -1; -1 8 -1; -1 -1 -1], imagem, "same");
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

inicio_extensao = length(argv(){2});
while(argv(){2}(inicio_extensao) != '.')
    inicio_extensao -= 1;
endwhile

nome_final = strcat(argv(){2}(1:inicio_extensao - 1), "-final.jpg");

if(strcmp(argv(){1}, "-contrast"))
    nova = equalizar_histograma(original);
elseif(strcmp(argv(){1}, "-blur"))
    nova = suavizar(original);
elseif(strcmp(argv(){1}, "-blur2"))
    nova = suavizar2(original);
elseif(strcmp(argv(){1}, "-sharpen"))
	nova = aumentar_nitidez(original);
elseif(strcmp(argv(){1}, "-sharpen2"))
    nova = aumentar_nitidez2(original);
else
    fprintf(stderr, "Metodo escolhido invalido. Encerrando.\n");
    exit();
endif
   
imwrite(nova, nome_final, "jpg");


