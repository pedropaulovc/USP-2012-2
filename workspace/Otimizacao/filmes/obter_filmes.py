import urllib2

ids_comedia = ['1298649', '1772341', '1217209', '1790886', '0107120']     
ids_acao = ['1074638', '1276104', '1386703', '0948470', '1258972']
ids_drama = ['1371111', '1024648', '1615065', '1907668', '1673434']

def baixa_categoria(categoria, lista):
	for id in lista:
		a = open(categoria + "/tt" + str(id), "w")
		f = urllib2.urlopen("http://www.omdbapi.com/?i=tt{0}&tomatoes=true".format(id))
		a.write(f.read())
		a.close()
		
baixa_categoria("comedia", ids_comedia)
baixa_categoria("acao", ids_acao)
baixa_categoria("drama", ids_drama)

