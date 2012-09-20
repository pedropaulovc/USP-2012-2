#! /usr/bin/octave -qf

## MAC0300 - 2012 - MÉTODOS NUMÉROCOS DA ÁLGEBRA LINEAR
## EP1 - TRANSCRIÇÃO AUTOMÁTICA DE MÚSICA MONOFÔNICA
##
## ALUNO: PEDRO PAULO VEZZÁ CAMPOS - 7538743
##
## Neste arquivo está implementado o código da que permite converter um arquivo
## .wav em um correspondente .midi conforme as especificações do EP. Função
## principal: executa().


1;

#Responsável por calcular a Transformada Discreta de Fourier utilizando o método
#da matriz DFT.
#Retorno: 
# dft = Resultado da transformada
# tempo = tempo necessário para calcular a transformada
#Parâmetros:
# - x: O vetor com amostras a ter a transformada calculada.
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

#Responsável por calcular as amplitudes de um espectro dado
#Retorno: 
# amp = o vetor de amplitudes
#Parâmetros:
# - espectro: O vetor contendo o espectro vindo da transformada de fourier
function [amp] = calcular_amplitudes(espectro)
	amp = abs(espectro);
endfunction

#Responsável por plotar um sinal dado (.wav) no tempo
#Retorno: 
# nenhum
#Parâmetros:
# - sinal: o sinal a ser plotado
# - tx_amostragem: a taxa de amostragem do sinal (44100 para wav)
# - arquivo: parâmetro opcional indicando que o plot deve ser feito no arquivo
#indicado
function plotar_tempo(sinal, tx_amostragem, arquivo)
	tempo = (1 : length(sinal))/tx_amostragem;
	plot(tempo, sinal);
	
	if(nargin > 2)
		print(arquivo, "-dpng");
	endif
endfunction

#Responsável por plotar um espectro dado no domínio das frequências
#Retorno: 
# nenhum
#Parâmetros:
# - amplitudes: as amplitudes calculadas do espectro 
# - tx_amostragem: a taxa de amostragem do sinal (44100 para wav)
# - arquivo: parâmetro opcional indicando que o plot deve ser feito no arquivo
#indicado
function plotar_frequencias(amplitudes, tx_amostragem, arquivo)
	N = length(amplitudes);
	
	plot(linspace(1/tx_amostragem, tx_amostragem/3, N/3), amplitudes(1:N/3));

	if(nargin > 2)
		print(arquivo, "-dpng");
	endif
endfunction

#Responsável por descobrir o evento midi equivalente à frequência fundamental
#de um som detectado através do espectro correspondente.
#Retorno: 
# fundamental = o evento MIDI correspondente à frequência fundamental detectada
#Parâmetros:
# - amplitudes: as amplitudes calculadas do espectro 
# - tx_amostragem: a taxa de amostragem do sinal (44100 para wav)
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

#Responsável por realizar uma busca binária na faixa de eventos MIDI representáveis
#e buscar o evento que melhor se adequa à frequência passada
#Retorno: 
# evento = o evento MIDI correspondente à frequência passada
#Parâmetros:
# - freq: a frequência a ter o evento MIDI descoberto
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

#Responsável por comparar números considerando erros de arredondamento.
# Parâmetros:
# - x: Primeiro número a ser comparado
# - y: Segundo número a ser comparado
# Retorno:
# 0 se |x - y| <= 1e-2
# 1 se x > y
# -1 caso contrário
function [comp] = compara(x, y)
	if (abs(x - y) <= 1e-2)
		comp = 0;
	elseif (x > y)
		comp = 1;
	else
		comp = -1;
	endif
endfunction

#Responsável por converter uma frequência para um evento MIDI realizando 
#arredondamentos necessários
#Retorno: 
# evento = o evento MIDI correspondente à frequência passada
#Parâmetros:
# - freq: a frequência a ser convertida
function [evento] = freq_para_midi(freq)
	evento = round(12 * log2(freq/440) + 69);
endfunction

#Responsável por converter um evento MIDI para uma frequência
#Retorno: 
# - freq: a frequência calculada
#Parâmetros:
# evento = o evento MIDI base para o cálculo
function [freq] = midi_para_freq(evento)
	freq = nthroot(2, 12) ^ (evento - 69) * 440;
endfunction

#Responsável por criar um arquivo MIDI com as informações passadas
#Retorno: 
# nenhum
#Parâmetros:
# arquivo = nome do arquivo MIDI a ser gravado
# notas = vetor contendo os eventos MIDI detectados
# inicio = vetor indicando o tempo de início da i-ésima nota
# fim = vetor indicando o tempo de fim da i-ésima nota
#Observação:
# Os tamanhos dos vetores devem ser iguais
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

#Responsável por criar um vetor deitado indicando na i-ésima posição quantas notas 
# já foram tocadas.
#Exemplo:
# [1 1 2 2 2 3] = 1 nota de 2 segundos, 1 nota de 3 segundos e 1 nota de 1 segundo
#Retorno: 
# tempos = o vetor explicado
#Parâmetros:
# sinal = o sinal (wav) a ter os tempos descobertos
# notas = vetor contendo os eventos MIDI detectados
# tx_amostragem: a taxa de amostragem do sinal (44100 para wav)
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

#Função principal do programa. Responsável por invocar as funções descritas acima
#para gerar o arquivo MIDI finalizado. Permite exibir os gráficos e escolher o
#algoritmo utilizado.
#Retorno: 
# tempos = o tempo de execução em segundos do algoritmo escolhido
# resultado = uma matriz deitada que indica na 1a linha os eventos MIDI detectados
# na segunda, os tempos de início dos eventos e na terceira os tempos de fim.
#Parâmetros:
# arquivo = o nome do arquivo .wav a ser analisado
# plotar = Se igual a "tela" ou "arquivo" imprime os gráficos no domínio do tempo
#para a música toda e no domínio da frequência para cada segundo. Outros valores
#inibem a geração de gráficos.
#algoritmo = "dft" para utilizar DFT ou "fft" para utilizar a FFT. Outros
#valores implicam no uso da FFT.
function [tempo, resultado] = executar(arquivo, plotar, algoritmo)
	if(strcmp(plotar, "arquivo"))
		figure ("visible", "off"); 
	endif
	
	nome = arquivo(1:(length(arquivo) - 4));
	intervalo = 44100;
	
	try
		[y, fs, nbits] = wavread(arquivo);
	catch 
		fprintf(stderr, "Não foi possível abrir o arquivo fornecido. Encerrando.\n");
		exit();
	end_try_catch
	
	if(strcmp(plotar, "arquivo") || strcmp(plotar, "tela"))
		plotar_tempo(y, fs, sprintf("%s_tempo.png", arquivo));
	endif
	
	tempos = descobrir_tempos(y, fs);
	segundos = floor(length(y) / intervalo);
	notas = [];
	inicio = [];
	fim = [];
	inicio_tmp = [];
	fim_tmp = [];
	notas_tmp = [];
	tempo = 0;
	
	mesmoBloco = false;
	for i = 0 : segundos - 1
		if(strcmp(algoritmo, "dft"))
			[x, parcial] = dft(y(i * intervalo + 1 : (i + 1) * intervalo));
			tempo += parcial;
		else
			tic();
			x = fft(y(i * intervalo + 1 : (i + 1) * intervalo));
			tempo += toc();
		endif
			
		amp = calcular_amplitudes(x);
		eventos = obter_fundamental(amp, fs);
		
		if(strcmp(plotar, "arquivo"))
			plotar_frequencias(amp, fs, sprintf("%s_freq_seg_%d.png", arquivo, i + 1));
		elseif(strcmp(plotar, "tela"))
			plotar_frequencias(amp, fs);
			drawnow;
		endif
				
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
	
	midi = strcat(nome, ".midi");
	escrever_midi(midi, notas, inicio, fim);
	
%	fid = fopen (strcat(nome, "_", algoritmo, ".txt"), "w");
%	fdisp (fid, strcat(nome, ";", algoritmo, ";"));
%	fdisp (fid, tempo);
%	fclose (fid);

endfunction

#Função que permite testar o funcionamento para todos os arquivos WAV disponíveis
#no diretório de execução do pŕograma. Exibe na saída padrão para cada arquivo
#o seu nome e os valores de retorno da função executa(arquivo, "nao", "fft").
#Parâmetros:
# nenhum
#Retorno:
# nenhum
function testar()
	wavs = dir ("*.wav");
	for i = 1:length(wavs)
		wavs(i).name
		executar(wavs(i).name, "nao", "fft")
	endfor
endfunction

if(nargin == 0)
	fprintf(stderr, "Forneca como argumento ao programa o arquivo a ser analisado.\n");
	fprintf(stderr, "Ex: octave %s lullaby.wav [plot] [alg]", program_name());
	return;
elseif(nargin == 1)
	[tempo, resultado] = executar(argv(){1}, "nao", "fft")
elseif(nargin == 2)
	[tempo, resultado] = executar(argv(){1}, argv(){2}, "fft")
else
	[tempo, resultado] = executar(argv(){1}, argv(){2}, argv(){3})
endif


