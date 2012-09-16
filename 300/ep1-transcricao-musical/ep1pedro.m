#! /bin/octave -qf


#• aplicar a Transformada de Fourier para cada segmento da onda, com intervalos de tempo deter-
#minados;
#• analisar o espectro de frequências e obter a frequência fundamental do intervalo;
#• determinar as notas e sobretons correspondentes a cada frequência fundamental; e
#• transcrever a música para um arquivo MIDI, criando eventos MIDI correspondentes a cada in-
#tervalo analisado.

1;

function [dft, tempo] = calcularDft(x)
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

function [amp] = calcularAmplitudes(quantR, quantI)
	amp = hypot(quantR, quantI);
endfunction

function plotarDominioTempo(sinal, amostragem)
	tempo = (1 : length(sinal))/amostragem;
	plot(tempo, sinal);
endfunction


function plotarDominioFrequencias(amplitudes, taxaAmostragem)
	N = length(amplitudes);
	
	plot(linspace(1/taxaAmostragem, taxaAmostragem, N), amplitudes);
endfunction

function [resposta] = obterEventos(amplitudes, taxaAmostragem)
	mediana = median(amplitudes);
	desvio = std(amplitudes);
	N = length(amplitudes);
	
	frequencias = [];
	for i = 1:N/3
		if amplitudes(i) > mediana + 3 * desvio;
			frequencias = [frequencias; i *  taxaAmostragem / N];
		endif
	endfor
	
	eventos = [];
	for i = 1:length(frequencias)
		eventos = union(eventos, buscarEvento(frequencias(i)));
	endfor
	
	contaHarmonicas = zeros(1, 12);
	for evento = eventos
		contaHarmonicas(mod(evento, 12) + 1) += 1;
	endfor
	
	[qtd harmonica] = max(contaHarmonicas);
	harmonica -= 1;
	
	resposta = [];
	for evento = eventos
		if(mod(evento, 12) == harmonica)
			resposta = [resposta, evento];
		endif
	endfor
	
endfunction

function [evento] = buscarEvento(freq)
	ini = 0;
	fim = 127;
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

function escreverMidi(nomeArquivo, notas, inicio, fim)
	% initialize matrix:
	M = zeros(length(notas),6);
	
	M(:,1) = 1;         % all in track 1
	M(:,2) = 1;         % all in channel 1
	M(:,3) = notas;      % note numbers
	M(:,4) = 100;       % volume
	M(:,5) = inicio;  % note on
	M(:,6) = fim;   % note off

	midi_new = matrix2midi(M);
	writemidi(midi_new, nomeArquivo);

endfunction

function [blocos] = descobrirBlocos(sinal, amostragem)
	segundos = floor(length(sinal) / amostragem);
	blocos = zeros(1, segundos);
	
	atual = 1;
	for i = 1 : segundos - 1
		blocos(i) = atual;
		if(abs(max(sinal(i * amostragem - 100 : i * amostragem + 100))) < 0.05)
			atual = atual + 1;
		endif
	endfor
	
	blocos(segundos) = atual;
endfunction

function [resultado] executar(nomeArquivo)
	# Decodificar as informações contidas no arquivo WAV;
	
	tamanhoIntervalo = 44100;
	
	try
		[y, fs, nbits] = wavread(nomeArquivo);
	catch 
		fprintf(stderr, "Não foi possível abrir o arquivo fornecido. Encerrando.\n");
		exit();
	end_try_catch
	
	blocos = descobrirBlocos(y, fs);
	segundos = floor(length(y) / tamanhoIntervalo);
	notas = [];
	inicio = [];
	fim = [];
	tmpInicio = [];
	tmpFim = [];
	tmpNotas = [];
	
	mesmoBloco = false;
	for i = 0 : segundos - 1
		x = fft(y(i * tamanhoIntervalo + 1 : (i + 1) * tamanhoIntervalo));
		
		amp = calcularAmplitudes(real(x), imag(x));
		eventos = obterEventos(amp, fs);
%		plotarDominioFrequencias(amp, fs);
%		drawnow;
%		pause();		
		
		mesmoBloco = false;
		if(i >= 1 && blocos(i) == blocos(i + 1))
			mesmoBloco = true;
		endif
		
		if(!mesmoBloco)
			notas = [notas; tmpNotas];
			inicio = [inicio; tmpInicio];
			fim = [fim; tmpFim];
			
			tmpNotas = eventos';
			tmpInicio = linspace(i, i, length(eventos))';
			tmpFim = tmpInicio + 1;
		else
			tmpFim = tmpFim + 1;
		endif
	endfor
	
	notas = [notas; eventos'];
	inicio = [inicio; tmpInicio];
	fim = [fim; tmpFim];

	resultado = [notas, inicio, fim]';
	
	nomeMidi = strcat(nomeArquivo(1:(length(nomeArquivo) - 3)), "midi");
	escreverMidi(nomeMidi, notas, inicio, fim);
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
	fprintf(stderr, "Ex: octave %s lullaby.wav\n", program_name());
	return;
else
	executar(argv(){1});
endif


