from django.http import HttpResponse
from django.shortcuts import render_to_response
from tweepy import Cursor, api

def home(request):
    return HttpResponse("Ola mundo!")
    
def termo(request, termo):
	try:
		resultado = list(Cursor(api.search, q=termo).items(20))
	except:
		resultado = [{'text': 'ERRO'}]
	
	return render_to_response('tweets.html', {'termo': termo, 'resultados': resultado})

