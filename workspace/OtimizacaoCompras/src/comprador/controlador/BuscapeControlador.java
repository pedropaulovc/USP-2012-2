package comprador.controlador;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.config.ClientConfig;
import com.sun.jersey.api.client.config.DefaultClientConfig;
import comprador.buscape.Result;
import java.io.InputStream;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;

/*
 * Classe para realizar requisições ao Buscappe
 * @author rodrigo
 * Last updated: Apr 15, 2012
 */
public class BuscapeControlador {
    public static String idApp= "6e7a5251666457573049493d";
    public static int maxResultados=20;

    private Client client;

    public BuscapeControlador() {
        ClientConfig config = new DefaultClientConfig();
        client = Client.create(config);
    } // construtor

    public Result acessaRecurso(String url) throws Exception {
        WebResource webResource = client.resource(url);
        ClientResponse response = webResource.get(ClientResponse.class);

        if (response.getStatus() != 200) {
            throw new RuntimeException("Erro na requisição. Código "
                    + response.getStatus());
        }

        InputStream in = response.getEntityInputStream();

        JAXBContext context = JAXBContext.newInstance("comprador.buscape");
        Unmarshaller unmarshaller = context.createUnmarshaller();
        Result result = (Result) unmarshaller.unmarshal(in);
        
        in.close();

        return result;
    } //acessaRecurso

    public Result acessaListaOfertas(int idProduto) throws Exception {
        String urlServico = "http://sandbox.buscape.com/service/findOfferList/";
        String parametros = idApp + "/br/?" + "productId=" + idProduto;
        String filtros = "&results=" + maxResultados;

        return acessaRecurso(urlServico+parametros+filtros);
    }

    public Result acessaListaOfertas(String palavraChave) throws Exception {
        String urlServico = "http://sandbox.buscape.com/service/findOfferList/";
        String parametros = idApp + "/br/?" + "keyword=" + palavraChave;
        String filtros = "&results=" + maxResultados;

        return acessaRecurso(urlServico+parametros+filtros);
    }


}
