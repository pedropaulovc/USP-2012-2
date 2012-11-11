import urllib2, re

generos = ['action', 'adventure', 'animation', 'biography', 'comedy', 'crime', 'documentary', 'drama', 'family', 'fantasy', 'film_noir', 'game_show', 'history', 'horror', 'music', 'musical', 'mystery', 'news', 'reality_tv', 'romance', 'sci_fi', 'sport', 'talk_show', 'thriller', 'war', 'western']

regex = re.compile("tt(\d+)")
for genero in generos:
	print "Baixando lista de filmes do genero " + genero
	url = "http://www.imdb.com/search/title?genres={0}&title_type=feature&sort=moviemeter,asc".format(genero)
	html = urllib2.urlopen(url).read()
	ids = set(regex.findall(html))
	for id in ids:
		arquivo = open("{0}/tt{1}".format(genero, id), "w")
		info = urllib2.urlopen("http://www.omdbapi.com/?i=tt{0}&tomatoes=true".format(id))
		arquivo.write(info.read())
		arquivo.close()

print "OK!"

