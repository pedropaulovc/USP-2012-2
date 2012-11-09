package comprador.controlador;

import comprador.buscape.OfferType;
import comprador.buscape.Result;
import comprador.modelo.ProdutoInfo;
import java.util.ArrayList;
import java.util.List;

/**
 * Gerencia uma lista de ProdutoInfo
 * @author rodrigo
 * Last updated: Apr 15, 2012
 */
public class ProdutoInfoControlador {

    /**
     * Recebe um objeto da API do Buscape e retorna uma lista com ProdutoInfo
     * @param result objeto contendo o resultado da busca no buscape
     * @return lista de ProdutoInfo
     */
    public static List<ProdutoInfo> retornaListaProdutos(Result result) {
        List<ProdutoInfo> produtos = new ArrayList<ProdutoInfo>();
        double preco=0.0, avaliacaoUsuario=0.0;
        String descricao="", loja="", avaliacaoEbit="";
        long id=(long) 0.0;
        int numComentarios=0;

        List<OfferType> ofertas = result.getOffer();
        for (OfferType oferta : ofertas) {
            try {
                id = oferta.getId();
                descricao = oferta.getOfferName();
                preco = Double.parseDouble(oferta.getPrice().getValue());
                loja = oferta.getSeller().getSellerName();
                avaliacaoUsuario = Double.parseDouble(oferta.getRating().getUserAverageRating().getRating());
                avaliacaoEbit = oferta.getRating().getEBitRating().getRating();
                numComentarios = oferta.getRating().getUserAverageRating().getNumComments();
            } catch (NullPointerException npe) {
            }

            ProdutoInfo p = new ProdutoInfo(id, preco, loja);
            p.setDescricao(descricao);
            p.setAvaliacaoUsuario(avaliacaoUsuario);
            p.setAvaliacaoEbit(avaliacaoEbit);
            p.setNumComentarios(numComentarios);

            produtos.add(p);
        } //for

        return produtos;
    }

}
