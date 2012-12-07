#!/usr/bin/octave -qf

## MAC0300 - 2012 - MÉTODOS NUMÉRICOS DA ÁLGEBRA LINEAR
## EP3 - COMPRESSÃO DE IMAGENS USANDO SVD
##
## ALUNO: PEDRO PAULO VEZZÁ CAMPOS - 7538743

1;

function [U, A, V] = golub_kahan_bidiagonalization(A)
	[m,n] = size(A);
	
	if m < n
		tmp = m;
		m = n;
		n = tmp;
		A = A';
	endif
	
	U = eye(m,m);
	V = eye(n,n);
	
	for k = 1:n
		x = A(k:m,k);
		x(1) = x(1) + sign(x(1)) * norm(x);
		u = x/norm(x);
		A(k:m,k:n) = A(k:m,k:n) - 2*u*( u'*A(k:m,k:n) );
		
		u_tmp = [zeros(m - rows(u),1);u];
		Qk = eye(m, m) - 2 * u_tmp * u_tmp';
		U = U * Qk;
		
		if k <= n-2
			x = A(k,k+1:n);
			x(1) = x(1) + sign(x(1)) * norm(x);
			v = x/norm(x);
			A(k:m,k+1:n) = A(k:m,k+1:n) - 2*( A(k:m,k+1:n)*v' )*v;
			
			v_tmp = [zeros(n - columns(v)),v];
			Pk1 = eye(n, n) - 2*v_tmp'*v_tmp;
			V = Pk1 * V;
		endif 
	endfor
endfunction

function [U, S, V] = golub_reinsch_svd(A)
	[m, n] = size(A)
	epsilon = erro()
	[U, B, V] = golub_kahan_bidiagonalization(A)
	
	while true
		for i = 1 : n - 1
			if abs(B(i, i + 1) <= epsilon * (abs(B(i,i)) + abs(B(i + 1, i + 1)))
				B(i, i + 1) = 0
			endif
			
			
		endfor
	endwhile

endfunction

function [B, Q, P] = golub_kahan_svd_step(A)

endfunction

function [e] = erro()
	e = sqrt(eps)
endfunction

if(nargin < 3)
	fprintf(stderr, "Forneca como argumento ao programa a imagem");
	fprintf(stderr, "e o valor do rank k da compressao.\n");
	fprintf(stderr, "Ex: octave %s -k 128 imagem.bmp\n", program_name());
		fprintf(stderr, "O programa salva, na pasta de execução do programa,");
	fprintf(stderr, "o arquivo da imagem processada em formato bmp, de,");
	fprintf(stderr, "nome igual ao arquivo de entrada, seguido do sufixo");
	fprintf(stderr, " compressed. Exemplo: imagem-compressed.bmp\n");
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


