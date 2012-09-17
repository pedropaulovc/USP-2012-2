#! /bin/octave -qf


1;

function [dft, tempo] = dft(x)
	u = zeros(1, length(x));
	dft = zeros(length(x), 1);
	tempo = 0;

	for i = 1 : length(x)
		u(i) = 1;
		w = fft(u);
		u(i) = 0;
		tic()
		dft(i) = w * x;
		tempo += toc();
	endfor
endfunction

function [amp] = calcular_amplitudes(espectro)
	amp = abs(espectro);
endfunction

function plotar_tempo(sinal, tx_amostragem)
	tempo = (1 : length(sinal))/tx_amostragem;
	plot(tempo, sinal);
endfunction


function plotar_frequencias(amplitudes, tx_amostragem)
	N = length(amplitudes);
	
	plot(linspace(1/tx_amostragem, tx_amostragem, N), amplitudes);
endfunction

function [fundamental] = obter_fundamental(amplitudes, tx_amostragem)
	mediana = median(amplitudes);
	desvio = std(amplitudes);
	N = length(amplitudes);
	
	frequencias = [];
	for i = 1:N/3
		if amplitudes(i) > mediana + 3 * desvio;
			frequencias = [frequencias; i *  tx_amostragem / N];
		endif
	endfor
	
	eventos = [];
	for i = 1:length(frequencias)
		eventos = union(eventos, buscar_midi(frequencias(i)));
	endfor
	
	cont_harmonicas = zeros(1, 12);
	acc_amplitudes = zeros(1, 12);
	for evento = eventos
		cont_harmonicas(mod(evento, 12) + 1) += 1;
		acc_amplitudes(mod(evento, 12) + 1) += amplitudes(floor(midi_para_freq(evento) * N / tx_amostragem));
	endfor
	
	harmonica = 1;
	for i = 1 : 12
		if( ( cont_harmonicas(i) > cont_harmonicas(harmonica) ) ||
			( cont_harmonicas(i) == cont_harmonicas(harmonica) && acc_amplitudes(i) > acc_amplitudes(harmonica) )
		)
			harmonica = i;
		endif
	endfor
	harmonica -= 1;
	
	for evento = eventos
		if(mod(evento, 12) == harmonica)
			fundamental = evento;
			return;
		endif
	endfor
	
endfunction

function [evento] = buscar_midi(freq)
	ini = 0;
	fim = 127;
	mid = floor((ini + fim) / 2);
	
	while(fim - ini > 1)
		mid = floor((ini + fim) / 2);
		
		comp = compara(freq, midi_para_freq(mid));
		if(comp == 0)
			evento = mid;
			return;
		elseif(comp < 0)
			fim = mid;
		else
			ini = mid;
		endif
	endwhile
	
	if(abs(freq - midi_para_freq(ini)) < abs(freq - midi_para_freq(fim)))
		evento = ini;
	else
		evento = fim;
	endif
endfunction

function [comp] = compara(x, y)
	if (abs(x - y) <= 1e-2)
		comp = 0;
	elseif (x > y)
		comp = 1;
	else
		comp = -1;
	endif
endfunction

function [evento] = freq_para_midi(freq)
	evento = round(12 * log2(freq/440) + 69);
endfunction

function [freq] = midi_para_freq(evento)
	freq = nthroot(2, 12) ^ (evento - 69) * 440;
endfunction

function escrever_midi(arquivo, notas, inicio, fim)
	% initialize matrix:
	M = zeros(length(notas),6);
	
	M(:,1) = 1;         % all in track 1
	M(:,2) = 1;         % all in channel 1
	M(:,3) = notas;      % note numbers
	M(:,4) = 100;       % volume
	M(:,5) = inicio;  % note on
	M(:,6) = fim;   % note off

	midi_new = matrix2midi(M);
	writemidi(midi_new, arquivo);

endfunction

function [tempos] = descobrir_tempos(sinal, tx_amostragem)
	segundos = floor(length(sinal) / tx_amostragem);
	tempos = zeros(1, segundos);
	
	atual = 1;
	for i = 1 : segundos - 1
		tempos(i) = atual;
		if(abs(max(sinal(i * tx_amostragem - 100 : i * tx_amostragem + 100))) < 0.05)
			atual = atual + 1;
		endif
	endfor
	
	tempos(segundos) = atual;
endfunction

function [resultado] = executar(arquivo)
	# Decodificar as informações contidas no arquivo WAV;
	
	intervalo = 44100;
	
	try
		[y, fs, nbits] = wavread(arquivo);
	catch 
		fprintf(stderr, "Não foi possível abrir o arquivo fornecido. Encerrando.\n");
		exit();
	end_try_catch
	
	tempos = descobrir_tempos(y, fs);
	segundos = floor(length(y) / intervalo);
	notas = [];
	inicio = [];
	fim = [];
	inicio_tmp = [];
	fim_tmp = [];
	notas_tmp = [];
	
	mesmoBloco = false;
	for i = 0 : segundos - 1
		x = fft(y(i * intervalo + 1 : (i + 1) * intervalo));
		
		amp = calcular_amplitudes(real(x), imag(x));
		eventos = obter_fundamental(amp, fs);
		%plotar_frequencias(amp, fs);
		%drawnow;
		%pause();		
		
		mesmoBloco = false;
		if(i >= 1 && tempos(i) == tempos(i + 1))
			mesmoBloco = true;
		endif
		
		if(!mesmoBloco)
			notas = [notas; notas_tmp];
			inicio = [inicio; inicio_tmp];
			fim = [fim; fim_tmp];
			
			notas_tmp = eventos';
			inicio_tmp = linspace(i, i, length(eventos))';
			fim_tmp = inicio_tmp + 1;
		else
			fim_tmp = fim_tmp + 1;
		endif
	endfor
	
	notas = [notas; eventos'];
	inicio = [inicio; inicio_tmp];
	fim = [fim; fim_tmp];

	resultado = [notas, inicio, fim]';
	
	midi = strcat(arquivo(1:(length(arquivo) - 3)), "midi");
	escrever_midi(midi, notas, inicio, fim);
endfunction

function testar()
	wavs = dir ("*.wav");
	for i = 1:length(wavs)
		wavs(i).name
		executar(wavs(i).name)
	endfor
endfunction

if(nargin < 1)
	fprintf(stderr, "Forneça como argumento ao programa o arquivo a ser analisado.\n");
	fprintf(stderr, "Ex: octave %s lullaby.wav [plot] [alg] [tempo]\n", program_name());
	return;
else
	executar(argv(){1});
endif


