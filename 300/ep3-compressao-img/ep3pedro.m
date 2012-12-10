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
		k = min(m, n)
		printf ("k invalido (Maior que menor das dimensões). Considerando k = %d\n", min(m,n))
	endif

	for i = 1:d
		[U,S,V] = svdcmp(img(:,:,i));
		l = min(k, rows(S));
		comp(:,:,i) = U(:,1:l) * S(1:l,1:l) * V(:,1:l)';
	endfor
	
	comp = uint8(comp);
endfunction

function [res] = sinal(a,b)
	if b >= 0.0
		res = abs(a);
	else
		res = -abs(a);
	endif
endfunction

function [a, w, v] = svdcmp(a)
	[m, n] = size(a);
	
	rv1 = zeros(n,1);
	w = zeros(n,1);
	v = zeros(n);
	
	g = scale = anorm = 0.0;
	% Householder reduction to bidiagonal form
	for i = 1:n
		l = i + 1;
		rv1(i) = scale * g;
		g = s = scale = 0.0;
		if (i <= m)
			for k = i:m	scale += abs(a(k,i)); endfor
			if scale
				for k = i:m
					a(k,i) /= scale;
					s += a(k,i) * a(k,i);
				endfor
				
				f = a(i,i);
				g = -sinal(sqrt(s),f);
				h = f * g - s;
				a(i,i) = f - g;
				for j = l:n
					s = 0.0;
					for k = i:m s += a(k,i) * a(k,j); endfor
					f = s/h;
					for k = i:m a(k,j) += f * a(k,i); endfor
				endfor
				for k = i:m a(k,i) *= scale; endfor
			endif
		endif
		
		w(i) = scale * g;
		g = s = scale = 0.0;
		
		if i <= m && i != n
			for k = l:n scale += abs(a(i,k)); endfor
			if scale
				for k = l:n
					a(i,k) /= scale;
					s += a(i,k) * a(i,k);
				endfor
				f = a(i,l);
				g = -sinal(sqrt(s),f);
				h = f * g - s;
				a(i,l) = f - g;
				for k = l:n rv1(k) = a(i,k)/h; endfor
				for j = l:m
					s = 0.0;
					for k = l:n s += a(j,k) * a(i,k); endfor
					for k = l:n a(j,k) += s * rv1(k); endfor
				endfor
				for k = l:n a(i,k) *= scale; endfor
			endif
		endif
		anorm = max(anorm, (abs(w(i)) + abs(rv1(i))));
	endfor
	
	
	% Accumulation of right-hand transformations.
	for i = n:-1:1
		if i < n
			if g
				for j = l:n %Double division to avoid possible underflow.
					v(j,i) = (a(i,j)/a(i,l))/g;
				endfor
				for j = l:n
					s = 0.0;
					for k = l:n s += a(i,k) * v(k,j); endfor
					for k = l:n v(k,j) += s * v(k,i); endfor
				endfor
			endif
			for j = l:n v(i,j) = v(j,i) = 0.0; endfor
		endif
		v(i,i) = 1.0;
		g = rv1(i);
		l = i;
	endfor
	% Accumulation of left-hand transformations.
	for i = min(m,n):-1:1
		l = i + 1;
		g = w(i);
		for j = l:n a(i,j) = 0.0; endfor
		if g
			g = 1.0/g;
			for j = l:n
				s = 0.0;
				for k = l:m s += a(k,i) * a(k,j); endfor
				f = (s/a(i,i)) * g;
				for k = i:m a(k,j) += f * a(k,i); endfor
			endfor
			for j = i:m a(j,i) *= g; endfor
		else
			for j = i:m a(j,i) = 0.0; endfor
		endif
		a(i,i) += 1;
	endfor
	% Diagonalization of the bidiagonal form.
	for k = n:-1:1
		for its = 1:30
			flag = 1;
			% Test for splitting.
			for l = k:-1:1
				nm = l - 1; % Note that rv1(1) is always zero.
				if abs(rv1(l)) + anorm == anorm
					flag = 0;
					break;
				endif
				if abs(w(nm)) + anorm == anorm break; endif
			endfor
			if flag
				c = 0.0; % Cancellation of rv1(l), if l > 1.
				s = 1.0;
				for i = l:k
					f = s * rv1(i);
					rv1(i) = c * rv1(i);
					if abs(f) + anorm == anorm break; endif
					g = w(i);
					h = hypot(f,g);
					w(i) = h;
					h = 1.0/h;
					c = g * h;
					s = -f * h;
					for j = 1:m
						y = a(j,nm);
						z = a(j,i);
						a(j,nm) = y * c + z * s;
						a(j,i) = z * c - y * s;
					endfor
				endfor
			endif
			z = w(k);
			% Convergence.
			if l == k
				% Singular value is made nonnegative.
				if z < 0.0
					w(k) = -z;
					for j = 1:n v(j,k) = -v(j,k); endfor
				endif
				break;
			endif
			if its == 30 printf("no convergence in 30 svdcmp iterations"); endif
			x = w(l); % Shift from bottom 2-by-2 minor.
			nm = k - 1;
			y = w(nm);
			g = rv1(nm);
			h = rv1(k);
			f = ((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y);
			g = hypot(f,1.0);
			f=((x-z)*(x+z)+h*((y/(f+sinal(g,f)))-h))/x;
			c = s = 1.0;
			% Next QR transformation:
			for j = l:nm
				i = j + 1;
				g = rv1(i);
				y = w(i);
				h = s * g;
				g = c * g;
				z = hypot(f, h);
				rv1(j) = z;
				c = f / z;
				s = h / z;
				f = x * c + g * s;
				g = g * c - x * s;
				h = y * s;
				y *= c;
				for jj = 1:n
					x = v(jj,j);
					z = v(jj,i);
					v(jj,j) = x * c + z * s;
					v(jj,i) = z * c - x * s;
				endfor
				z = hypot(f,h);
				w(j) = z; % Rotation can be arbitrary if z = 0.
				if z
					z = 1.0 / z;
					c = f * z;
					s = h * z;
				endif
				f = c * g + s * y;
				x = c * y - s * g;
				for jj = 1:m
					y=a(jj,j);
					z=a(jj,i);
					a(jj,j) = y * c + z * s;
					a(jj,i) = z * c - y * s;
				endfor
			endfor
			rv1(l) = 0.0;
			rv1(k) = f;
			w(k) = x;
		endfor
	endfor
	
	[w, perm] = sort(w, 'descend');
	w = diag(w);
	a = a(:, perm);
	v = v(:, perm);
	
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


