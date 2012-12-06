#!/usr/bin/octave -qf

## MAC0300 - 2012 - MÉTODOS NUMÉROCOS DA ÁLGEBRA LINEAR
## EP3 - COMPRESSÃO DE IMAGENS USANDO SVD
##
## ALUNO: PEDRO PAULO VEZZÁ CAMPOS - 7538743

1;


if(nargin < 3)
	fprintf(stderr, "Forneca como argumento ao programa a imagem");
	fprintf(stderr, "e o valor do rank k da compressao.\n");
	fprintf(stderr, "Ex: octave %s -k 128 imagem.bmp\n", program_name());
    fprintf(stderr, "O programa salva, na pasta de execução do programa,");
	fprintf(stderr, "o arquivo da imagem processada em formato bmp, de,");
	fprintf(stderr, "nome igual ao arquivo de entrada, seguido do sufixo");
	fprintf(stderr, "compressed. Exemplo: imagem-compressed.bmp\n");
	return;
endif

try
	original = imread(argv(){3});
catch 
	fprintf(stderr, "Não foi possível abrir o arquivo fornecido. Encerrando.\n");
	exit();
end_try_catch

inicio_nome = length(argv(){3});
while(inicio_nome >= 1 && argv(){3}(inicio_nome) != '/')
    inicio_nome -= 1;
endwhile

inicio_extensao = length(argv(){3});
while(argv(){3}(inicio_extensao) != '.')
    inicio_extensao -= 1;
endwhile

nome_final = strcat(argv(){3}(inicio_nome + 1:inicio_extensao - 1), "-compressed.bmp");

k = argv(){2}
nova = original;


imwrite(nova, nome_final, "bmp");


