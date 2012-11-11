/*
 * This file is part of JGAP.
 *
 * JGAP offers a dual license model containing the LGPL as well as the MPL.
 *
 * For licensing information please see the file license.txt included with JGAP
 * or have a look at the top of class org.jgap.Chromosome which representatively
 * includes the JGAP license policy applicable for any file delivered with JGAP.
 *
 * Tradução par ao português.
 */
package moedas;

import org.jgap.*;

/**
 * Exemplo de função de custo para o exemplo MakeChange.
 *
 * Autores originais:
 * @author Neil Rotstan
 * @author Klaus Meffert
 * @since 1.0
 */
public class MinimizingMakeChangeFitnessFunction
    extends FitnessFunction {
  /** String com o número da revisão */
  private final static String CVS_REVISION = "$Revision: 1.18 $";

  private final int m_targetAmount;

  public static final int MAX_BOUND = 4000;
  public static final int MAX_AMOUNT = 100;

  public MinimizingMakeChangeFitnessFunction(int a_targetAmount) {
    if (a_targetAmount < 1 || a_targetAmount >= MAX_AMOUNT) {
      throw new IllegalArgumentException(
          "O valor deve estar entre 1 e " + MAX_AMOUNT + " centavos");
    }
    m_targetAmount = a_targetAmount;
  }

  /**
   * Determina o custo de uma instância de cromossomo. O valor mais alto
   * é o que mais se adequa. Este método deveria sempre retornar o mesmo valor
   * para dois cromossomos equivalentes.
   *
   * @param a_subject a instância de cromossomo a ser avaliada.
   *
   * @return um número double positivo que reflete o custo do cromossomo
   * @since 2.0 (until 1.1: return type int)
   * @author Neil Rotstan, Klaus Meffert, John Serri
   */
  public double evaluate(IChromosome a_subject) {
    /*
     * Cuidado com o avaliador de custo (fitness). Ele poderia ser o melhor com
     * valores maiores (e.g.DefaultFitnessEvaluator). Ou poderia ser o melhor com
     * valores menores, pois o valor de custo é visto como uma taxa de defeito
     * (e.g. DeltaFitnessEvaluator)
     */
    boolean defaultComparation = a_subject.getConfiguration().
        getFitnessEvaluator().isFitter(2, 1); //true, se o primeiro é maior que o segundo

    /*
     * O valor de custo mede quão próximo o valor está do total fornecido
     * pelo usuário e também o número total de moedas usadas na solução.
     * É realizado em dois passos: Passo 1: representar a quantidade de troco
     * versus a quantidade final do troco e retornar valores maiores para quantidades
     * mais próximas do total e valores menores para quantidades distantes ao total.
     * Passo 2: retorna valores maiorres para soluções próximas com poucas moedas e
     * valores menores para soluções com mais moedas.
     */
    int changeAmount = amountOfChange(a_subject);
    int totalCoins = getTotalNumberOfCoins(a_subject);
    int changeDifference = Math.abs(m_targetAmount - changeAmount);

    double fitness = defaultComparation ? 0.0d : MAX_BOUND/2;

    /*
     * Passo 1: Determina a distância da solução do total. Se a diferença
     * é maior que zero, nós dividiremos pela diferença no troco entre o
     * valor encontrado e o total. Isto garente devolver altos valores para
     * quantidades próximos ao total e baixo valores para quantidades distantes
     * do total. Se for zero, encontramos a diferença exata e associamos um
     * valor alto.
     */
    if (defaultComparation) {
      fitness += changeDifferenceBonus(MAX_BOUND/2, changeDifference);
    } else {
      fitness -= changeDifferenceBonus(MAX_BOUND/2, changeDifference);
    }

    /*
     * Passo 2: Nós dividimos o valor de custo por uma penalidade baseada no
     * número de moedas. Quanto maior o número de moedas maior é a penalidade.
     */
    if (defaultComparation) {
      fitness -= computeCoinNumberPenalty(MAX_BOUND/2, totalCoins);
    } else {
      fitness += computeCoinNumberPenalty(MAX_BOUND/2, totalCoins);
    }
    
    // Tenha certeza que o valor de custo é sempre positivo
    return Math.max(1.0d, fitness);
  }

  /**
   * Cálculo do bonus do valor de custo
   * @param a_maxFitness máximo valor de custo aplicável.
   * @param a_changeDifference diferença de troco em moedas para o problema moedas.
   * @return bonus da diferença.
   *
   * @author Klaus Meffert
   * @since 2.3
   */
  protected double changeDifferenceBonus(double a_maxFitness,
                                         int a_changeDifference) {
    if (a_changeDifference == 0) {
      return a_maxFitness;
    } else {
      /* nós arbitrariamente trabalhamos com metade do máximo de custo como base
        para soluções não ótimas. */
      if (a_changeDifference * a_changeDifference >= a_maxFitness / 2) {
        return 0.0d;
      } else {
        return a_maxFitness / 2 - a_changeDifference * a_changeDifference;
      }
    }
  }

  /**
   * Calcula a penalidade para aplicar ao custo baseado na quantidade de moedas
   * na solução.
   *
   * @param a_maxFitness valor máximo de custo permitido
   * @param a_coins número de moedas na solução
   * @return penalidade para o valor de custo baseado na quantidade de moedas
   *
   * @author John Serri
   * @since 2.2
   */
  protected double computeCoinNumberPenalty(double a_maxFitness, int a_coins) {
    if (a_coins == 1) {
      // a solução não pode ter menos que uma moeda.
      return 0;
    }
    else {
      /* Mais moedas então maior penalidade, mas não mais que o custo máximo
      para o valor possível. Vamos evitar o comportamento linear e usar
      penalidade exponencial. */
      return (Math.min(a_maxFitness, a_coins * a_coins));
    }
  }

  /**
   * Calcula o total de troco em centavos representado por uma dada solução
   * e retorna essa diferença.
   *
   * @param a_potentialSolution solução potencial a ser avaliada.
   * @return o total de troco em centavos de uma solução.
   *
   * @author Neil Rotstan
   * @since 1.0
   */
  public static int amountOfChange(IChromosome a_potentialSolution) {
    int numQuarters = getNumberOfCoinsAtGene(a_potentialSolution, 0);
    int numDimes = getNumberOfCoinsAtGene(a_potentialSolution, 1);
    int numNickels = getNumberOfCoinsAtGene(a_potentialSolution, 2);
    int numPennies = getNumberOfCoinsAtGene(a_potentialSolution, 3);
    return (numQuarters * 25) + (numDimes * 10) + (numNickels * 5) +
        numPennies;
  }

  /**
   * Recupera o número de moedas de uma solução dado a posição de um gene
   *
   * @param a_potentialSolution solução potencial (cromossomo)
   * @param a_position a posição do gene
   * @return o número de moedas
   *
   * @author Neil Rotstan
   * @since 1.0
   */
  public static int getNumberOfCoinsAtGene(IChromosome a_potentialSolution,
                                           int a_position) {
    return (Integer)a_potentialSolution.getGene(a_position).getAllele();
  }

  /**
   * Retorna o numero total de moedas
   *
   * @param a_potentialsolution solução potencial (cromossomo)
   * @return Retorna o número total de moedas.
   *
   * @author Neil Rotstan
   * @since 1.0
   */
  public static int getTotalNumberOfCoins(IChromosome a_potentialsolution) {
    int totalCoins = 0;
    int numberOfGenes = a_potentialsolution.size();
    for (int i = 0; i < numberOfGenes; i++) {
      totalCoins += getNumberOfCoinsAtGene(a_potentialsolution, i);
    }
    return totalCoins;
  }
}
