package comprador.modelo;

/**
 * Representa um Alelo
 * @author rodrigo
 * Last updated: Apr 14, 2012
 */
public class ProdutoInfo implements Comparable<ProdutoInfo>{
    private long idProduto;
    private String descricao;
    private double preco;
    private String loja;
    private double avaliacaoUsuario;
    private String avaliacaoEbit;
    private int numComentarios;

    public ProdutoInfo () {
    }

    public ProdutoInfo(long id, double preco, String loja) {
        this.idProduto = id;
        this.preco = preco;
        this.loja = loja;
    }

    public String getDescricao() {
        return descricao;
    }

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public long getIdproduto() {
        return idProduto;
    }

    public void setIdproduto(long idproduto) {
        this.idProduto = idproduto;
    }

    public String getLoja() {
        return loja;
    }

    public void setLoja(String loja) {
        this.loja = loja;
    }

    public String getAvaliacaoEbit() {
        return avaliacaoEbit;
    }

    public void setAvaliacaoEbit(String avaliacaoEbit) {
        this.avaliacaoEbit = avaliacaoEbit;
    }

    public double getAvaliacaoUsuario() {
        return avaliacaoUsuario;
    }

    public void setAvaliacaoUsuario(double avaliacaoUsuario) {
        this.avaliacaoUsuario = avaliacaoUsuario;
    }

    public double getPreco() {
        return preco;
    }

    public void setPreco(double preco) {
        this.preco = preco;
    }

    public int getNumComentarios() {
        return numComentarios;
    }

    public void setNumComentarios(int numComentarios) {
        this.numComentarios = numComentarios;
    }

    public int compareTo(ProdutoInfo o) {
        if (this.preco == preco) {
            return 0;
        }
        return this.preco>o.preco ? 1 : -1;
    }

    @Override
    public String toString() {
        return "{" + loja + "," + preco + "," + avaliacaoUsuario + "," + avaliacaoEbit + '}';
    }


}
