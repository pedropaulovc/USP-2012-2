#!/usr/bin/octave -qf

## MAC0300 - 2012 - MÉTODOS NUMÉRICOS DA ÁLGEBRA LINEAR
## EP3 - COMPRESSÃO DE IMAGENS USANDO SVD
##
## ALUNO: PEDRO PAULO VEZZÁ CAMPOS - 7538743

1;

function [comp] = comprimir(img, k)
	[m, n, d] = size(img);
	img = double(img);
	
	if k > min(m, n)
		k = min(m, n);
		printf ("k invalido (Maior que menor das dimensões). Considerando k = %d\n", min(m,n))
	endif

	for i = 1:d
		[U,S,V] = golub_reinsch(img(:,:,i));
		l = min(k, rows(S));
		comp(:,:,i) = U(:,1:l) * S(1:l,1:l) * V(:,1:l)';
	endfor
	
	comp = uint8(comp);
endfunction

function testar_svd()
	A = rand(12,6);
	[U,S,V] = golub_reinsch(A);
	cond(A - U * S * V')
	A = magic(10);
	[U,S,V] = golub_reinsch(A);
	cond(A - U * S * V')
	A = rand(6,12);
	[U,S,V] = golub_reinsch(A);
	cond(A - U * S * V')
endfunction

function [A, S, V] = golub_reinsch(A)
	[m, n] = size(A);
	
	tmp = zeros(n,1);
	S = zeros(n,1);
	V = zeros(n);
	
	g = escala = anorm = 0.0;
	% Gravando na matriz 'S' a transformação da matriz 'A' à forma bidiagonal 
	% através de reflexões de Householder
	for i = 1:n
		l = i + 1;
		tmp(i) = escala * g;
		g = s = escala = 0.0;
		if (i <= m)
			escala = norm(A(i:m, i), 1);
			if escala
				A(i:m,i) /= escala;
				s = A(i:m,i)' * A(i:m,i);
			
				f = A(i,i);
				g = sqrt(s) * -sign(f);
				h = f * g - s;
				A(i,i) = f - g;
				for j = l:n
					s = A(i:m,i)' * A(i:m,j);
					f = s/h;
					A(i:m,j) += f * A(i:m,i);
				endfor
				A(i:m,i) *= escala;
			endif
		endif
		
		S(i) = escala * g;
		g = s = escala = 0.0;
		
		if i <= m && i != n
			escala = norm(A(i,l:n), 1);
			if escala
				A(i,l:n) /= escala;
				s = A(i,l:n) * A(i, l:n)';
				f = A(i,l);
				g = sqrt(s) * -sign(f);
				h = f * g - s;
				A(i,l) = f - g;
				tmp(l:n) = A(i,l:n)/h;

				for j = l:m
					s = A(j,l:n) * A(i,l:n)';
					A(j,l:n) += s * tmp(l:n)';
				endfor
				A(i,l:n) *= escala;
			endif
		endif
		anorm = max(anorm, (abs(S(i)) + abs(tmp(i))));
	endfor
	
	
	% Montando a matriz 'V' com as transformações de Householder utilizadas à direita
	for i = n:-1:1
		if i < n
			if g
				V(l:n,i) = (A(i,l:n)/A(i,l))/g';
				for j = l:n
					s = A(i,l:n) * V(l:n,j);
					V(l:n,j) += s * V(l:n,i);
				endfor
			endif
			V(i,l:n) = 0.0;
			V(l:n,i) = 0.0;
		endif
		V(i,i) = 1.0;
		g = tmp(i);
		l = i;
	endfor
	
	
	% Atualizando a matriz 'A' (Que representa U na decomposição A = USV')
	% com as transformações de Householder utilizadas à esquerda
	for i = min(m,n):-1:1
		l = i + 1;
		g = S(i);
		A(i,l:n) = 0.0;
		if g
			g = 1.0/g;
			for j = l:n
				s = A(l:m,i)' * A(l:m,j);
				f = (s/A(i,i)) * g;
				A(i:m,j) += f * A(i:m,i);
			endfor
			A(i:m,i) *= g;
		else
			A(i:m,i) = 0.0;
		endif
		A(i,i) += 1;
	endfor
	
	% Transformando 'S' de bidiagonal para diagonal através de rotações de Givens
	for k = n:-1:1
		for its = 1:30
			flag = 1;
			for l = k:-1:1
				nm = l - 1;
				if abs(tmp(l)) + anorm == anorm
					flag = 0;
					break;
				endif
				if abs(S(nm)) + anorm == anorm break; endif
			endfor
			if flag
				c = 0.0;
				s = 1.0;
				for i = l:k
					f = s * tmp(i);
					tmp(i) = c * tmp(i);
					if abs(f) + anorm == anorm break; endif
					g = S(i);
					h = hypot(f,g);
					S(i) = h;
					h = 1.0/h;
					c = g * h;
					s = -f * h;
					
					y = A(1:m,nm);
					z = A(1:m,i);
					A(1:m,nm) = y * c + z * s;
					A(1:m,i) = z * c - y * s;
				endfor
			endif
			z = S(k);
			% Houve convergência.
			if l == k
				% Transformando valores singulares em não-negativos
				if z < 0.0
					S(k) = -z;
					V(1:n,k) *= -1;
				endif
				break;
			endif
			if its == 30 printf("Não houve convergência após 30 iterações da SVD\n"); endif
			x = S(l);
			nm = k - 1;
			y = S(nm);
			g = tmp(nm);
			h = tmp(k);
			f = ((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y);
			g = hypot(f,1.0);
			f=((x-z)*(x+z)+h*((y/(f+g*sign(f)))-h))/x;	
			c = s = 1.0;
			% Aplicando as transformações de Givens:
			for j = l:nm
				i = j + 1;
				g = tmp(i);
				y = S(i);
				h = s * g;
				g = c * g;
				z = hypot(f, h);
				tmp(j) = z;
				c = f / z;
				s = h / z;
				f = x * c + g * s;
				g = g * c - x * s;
				h = y * s;
				y *= c;
				
				x = V(1:n,j);
				z = V(1:n,i);
				V(1:n,j) = x * c + z * s;
				V(1:n,i) = z * c - x * s;
				z = hypot(f,h);
				S(j) = z;
				if z
					z = 1.0 / z;
					c = f * z;
					s = h * z;
				endif
				f = c * g + s * y;
				x = c * y - s * g;

				y=A(1:m,j);
				z=A(1:m,i);
				A(1:m,j) = y * c + z * s;
				A(1:m,i) = z * c - y * s;
			endfor
			tmp(l) = 0.0;
			tmp(k) = f;
			S(k) = x;
		endfor
	endfor
	
	[S, perm] = sort(S, 'descend');
	S = diag(S);
	A = A(:, perm);
	V = V(:, perm);
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

k = str2double(argv(){2});
nova = comprimir(original, k);

imwrite(nova, nome_final, "bmp");


