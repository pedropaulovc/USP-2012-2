#! /bin/octave -qf


#• aplicar a Transformada de Fourier para cada segmento da onda, com intervalos de tempo deter-
#minados;
#• analisar o espectro de frequências e obter a frequência fundamental do intervalo;
#• determinar as notas e sobretons correspondentes a cada frequência fundamental; e
#• transcrever a música para um arquivo MIDI, criando eventos MIDI correspondentes a cada in-
#tervalo analisado.

1;

function [U, V] = montaDft(N)
	U = zeros(N, N); #Parte real da DFT
	V = zeros(N, N); #Parte imaginária da DFT
	
	raizN = 1; #TODO Trocar para sqrt(N)
	for m = 0 : N - 1
		for n = 0: N - 1
			w = (2 * pi * m * n)/N;
			U(m + 1, n + 1) = cos(w);
			V(m + 1, n + 1) = -sin(w);
		endfor
	endfor
endfunction

function plotarDominioTempo(sinal, amostragem)
	tempo = (0 : length(sinal))/amostragem;
	plot(tempo, sinal);
endfunction

function [amp] = calcularAmplitudes(quantR, quantI)
	amp = zeros(length(quantR), 1);
	for i = 1 : length(quantR)
		amp(i) = quantR(i) * quantR(i) + quantI(i) * quantI(i);
	endfor
endfunction

function plotarDominioFrequencias(amplitudes)
	plot(1:length(amplitudes), amplitudes);
endfunction
	
#function executar()
	if(nargin < 1)
		fprintf(stderr, "Forneça como argumento ao programa o arquivo a ser analisado.\n");
		fprintf(stderr, "Ex: octave %s lullaby.wav\n", program_name());
		return;
	endif

	nomeArquivo = argv(){1};

	# Decodificar as informações contidas no arquivo WAV;
	try
		[y, fs, nbits] = wavread(nomeArquivo);
	catch 
		fprintf(stderr, "Não foi possível abrir o arquivo fornecido. Encerrando.\n");
		exit();
	end_try_catch
	
	x = fft(y);
	
	rx = real(x);
	ix = imag(x);
	amp = calcularAmplitudes(rx, ix)
	plotarDominioFrequencias(amp)
#endfunction

