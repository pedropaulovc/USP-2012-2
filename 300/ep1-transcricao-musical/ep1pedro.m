#! /bin/octave -qf


#• aplicar a Transformada de Fourier para cada segmento da onda, com intervalos de tempo deter-
#minados;
#• analisar o espectro de frequências e obter a frequência fundamental do intervalo;
#• determinar as notas e sobretons correspondentes a cada frequência fundamental; e
#• transcrever a música para um arquivo MIDI, criando eventos MIDI correspondentes a cada in-
#tervalo analisado.

1;

function [U, V] = montarMatrizDft(N)
	U = zeros(N, N); #Parte real da DFT
	V = zeros(N, N); #Parte imaginária da DFT
	
	raizN = sqrt(N);
	for m = 0 : N - 1
		for n = 0: N - 1
			w = -(2 * pi * m * n)/N;
			U(m + 1, n + 1) = cos(w)/raizN;
			V(m + 1, n + 1) = -sin(w)/raizN;
		endfor
	endfor
endfunction

function [U, V] = calcularDft(x)
	N = length(x);
	U = zeros(N, 1);
	V = zeros(N, 1);
	
	for m = 0 : N - 1
		for n = 0 : N - 1
			w = -(2 * pi * m * n)/N;
			U += x(n + 1) * cos(w);
			V += x(n + 1) * sin(w);
		endfor
	endfor
endfunction


function [amp] = calcularAmplitudes(quantR, quantI)
	amp = hypot(quantR, quantI);
endfunction

function plotarDominioTempo(sinal, amostragem)
	tempo = (0 : length(sinal))/amostragem;
	plot(tempo, sinal);
endfunction


function plotarDominioFrequencias(amplitudes, taxaAmostragem)
	N = length(amplitudes);
	
	plot(linspace(1/taxaAmostragem, taxaAmostragem, N), amplitudes);
endfunction

function [eventos] = obterEventos(amplitudes, taxaAmostragem)
	media = mean(amplitudes);
	desvio = std(amplitudes);
	N = length(amplitudes);
	
	frequencias = [];
	for i = 1:N/2
		if amplitudes(i) > media + 7 * desvio;
			frequencias = [frequencias; i *  taxaAmostragem / N];
		endif
	endfor
	
	eventos = [];
	for i = 1:length(frequencias)
		eventos = union(eventos, buscarEvento(frequencias(i)));
	endfor
	
endfunction

function [evento] = buscarEvento(freq)
	ini = 12;
	fim = 143;
	mid = floor((ini + fim) / 2);
	
	while(fim - ini > 1)
		mid = floor((ini + fim) / 2);
		
		comp = compare(freq, obterFrequencia(mid));
		if(comp == 0)
			evento = mid;
			return;
		elseif(comp < 0)
			fim = mid;
		else
			ini = mid;
		endif
	endwhile
	
	if(abs(freq - obterFrequencia(ini)) < abs(freq - obterFrequencia(fim)))
		evento = ini;
	else
		evento = fim;
	endif
endfunction

function [comp] = compare(x, y)
	if (abs(x - y) <= 1e-2)
		comp = 0;
	elseif (x > y)
		comp = 1;
	else
		comp = -1;
	endif
endfunction

function [evento] = obterEventoMidi(freq)
	evento = round(12 * log2(freq/440) + 69);
endfunction

function [freq] = obterFrequencia(evento)
	freq = nthroot(2, 12) ^ (evento - 69) * 440;
endfunction

function escreverMidi(nomeArquivo, musica)
	% initialize matrix:
	N = rows(musica) * columns(musica);  % number of notes
	M = zeros(N,6);
	
	notas = [];
	temposIni = [];
	temposFim = [];
	for i = 1 : rows(musica)
		notas = [notas; musica(i, :)'];
		temposIni = [temposIni; linspace(i - 1, i - 1, columns
	endfor
	
	M(:,1) = 1;         % all in track 1
	M(:,2) = 1;         % all in channel 1
	M(:,3) = notas;      % note numbers: one ocatave starting at middle C (60)
	M(:,4) = 100;  % lets have volume ramp up 80->120
	M(:,5) = (.5:.5:6.5)';  % note on:  notes start every .5 seconds
	M(:,6) = M(:,5) + 1;   % note off: each note has duration .5 seconds

	midi_new = matrix2midi(M);
	writemidi(midi_new, nomeArquivo);

endfunction

function executar(nomeArquivo)
	# Decodificar as informações contidas no arquivo WAV;
	
	tamanhoIntervalo = 44100;
	
	try
		[y, fs, nbits] = wavread(nomeArquivo);
	catch 
		fprintf(stderr, "Não foi possível abrir o arquivo fornecido. Encerrando.\n");
		exit();
	end_try_catch
	
	partes = floor(length(y) / tamanhoIntervalo);
	musica = []; %linha 1 tem as notas, linha 2 tem o início
	for i = 0 : partes - 1
		ini = i * tamanhoIntervalo + 1;
		fim = (i + 1) * tamanhoIntervalo;
		x = fft(y(ini : fim));
		
		amp = calcularAmplitudes(real(x), imag(x));
%		plotarDominioFrequencias(amp, fs)
		eventos = obterEventos(amp, fs);
		
		trecho = [eventos; linspace(i, i, length(eventos)];
		musica = [musica, trecho];
	endfor
	
	nomeMidi = strcat(nomeArquivo(1:(length(nomeArquivo) - 3)), "midi");
	escreverMidi(nomeMidi, musica);
endfunction

if(nargin < 1)
	fprintf(stderr, "Forneça como argumento ao programa o arquivo a ser analisado.\n");
	fprintf(stderr, "Ex: octave %s lullaby.wav\n", program_name());
	return;
else
	executar(argv(){1});
endif


