/*
 * This file is part of JGAP.
 *
 * JGAP offers a dual license model containing the LGPL as well as the MPL.
 *
 * For licensing information please see the file license.txt included with JGAP
 * or have a look at the top of class org.jgap.Chromosome which representatively
 * includes the JGAP license policy applicable for any file delivered with JGAP.
 *
 * Tradução para o português.
 */
package moedas;

import java.awt.HeadlessException;
import java.io.*;
import javax.swing.JOptionPane;

import org.jgap.*;
import org.jgap.data.*;
import org.jgap.impl.*;
import org.jgap.xml.*;
import org.w3c.dom.*;

/**
 * Implementação do problema clássico "Make change" (Calcular o troco)
 * usando algoritmos genéticos. O objetivo é dar o troco usando o número mínimo
 * de moedas. Esta implementação foi modificada para aceitar as moedas no Brasil
 * (R$ 0,01, R$ 0,05, R$ 0,10 e R$0,25).
 *
 * Autores originais:
 * @author Neil Rotstan
 * @author Klaus Meffert
 * @since 1.0
 */
public class MinimizingMakeChange {

    /** Contém a revisão CVS */
    private final static String CVS_REVISION = "$Revision: 1.27 $";
    /** Número total de vezes que a população evolui.*/
    private static final int MAX_ALLOWED_EVOLUTIONS = 50;

    /**
     * Executa o algoritmo genético para determinar o número mínimo de moedas
     * para o troco.
     *
     * @param a_targetChangeAmount o valor do troco
     * @param a_doMonitor true: habilita o monitor para avaliar o progresso
     * de evolução
     *
     * @throws Exception
     *
     * @author Neil Rotstan
     * @author Klaus Meffert
     * @since 1.0
     */
    public static void makeChangeForAmount(int a_targetChangeAmount)
            throws Exception {
        /* inicia com a DefaultConfiguration (configurações mais comuns) */
        Configuration conf = new DefaultConfiguration();

        /* mantém para a próxima geração a melhor população */
        conf.setPreservFittestIndividual(true);

        /* permite que o tamanho da população possa aumentar */
        conf.setKeepPopulationSizeConstant(false);

        /* inicializa e configura a função de custo */
        FitnessFunction myFunc =
                new MinimizingMakeChangeFitnessFunction(a_targetChangeAmount);
        conf.setFitnessFunction(myFunc);

        /*
        Criar um cromossomo com 4 genes (1 para cada tipo de moeda)
        Foi usada a classe IntegerGene para representar a quantidade
        máxima de moedas de cada tipo.
         */
        Gene[] sampleGenes = new Gene[4];
        sampleGenes[0] = new IntegerGene(conf, 0, 4);  // R$ 0,25
        sampleGenes[1] = new IntegerGene(conf, 0, 2);  // R$ 0,10
        sampleGenes[2] = new IntegerGene(conf, 0, 1);  // R$ 0,5
        sampleGenes[3] = new IntegerGene(conf, 0, 5);  // R$ 0,01

        /* cria e configura o cromossomo */
        IChromosome sampleChromosome = new Chromosome(conf, sampleGenes);
        conf.setSampleChromosome(sampleChromosome);

        /* Definir o tamanho da população, ou seja, quantos cromossomos
         * compõem a população. Quanto mais cromossomos, maior o número
         * de soluções potenciais, mas maior o tempo para evoluir a população.
         */
        conf.setPopulationSize(20);

        /*
         * criar uma população inicial aleatória de cromossomos.
         * pode ser lido de um arquivo (mas não foi!).
         */
        Genotype population;
        population = Genotype.randomInitialGenotype(conf);

        /*
        Evolui a população. Como não sabemos a melhor resposta faremos a evolução
        um número finito de vezes.
         */
        long startTime = System.currentTimeMillis();
        for (int i = 0; i < MAX_ALLOWED_EVOLUTIONS; i++) {
            if (!uniqueChromosomes(population.getPopulation())) {
                throw new RuntimeException("Estado inválido na geração." + i);
            }
            population.evolve();
        }
        long endTime = System.currentTimeMillis();

        /* grava o estado final da população em um arquivo xml */
        saveStateInFile(population, "GenotypeTroco");

        /* Apresenta a melhor solução */
        showBestSolutionFound(population, endTime, startTime);
    }

    private static void showBestSolutionFound(Genotype population, long endTime, long startTime) throws HeadlessException {
        IChromosome bestSolutionSoFar = population.getFittestChromosome();
        String resposta = "Tempo total de evolução: " + (endTime - startTime) +
               " ms\n\n" + "A melhor solução encontrada tem valor de " +
               bestSolutionSoFar.getFitnessValue() + "\nE contém:\n\t" +
               MinimizingMakeChangeFitnessFunction.getNumberOfCoinsAtGene(bestSolutionSoFar, 0) +
               " de R$ 0,25. \n\t" +
               MinimizingMakeChangeFitnessFunction.getNumberOfCoinsAtGene(bestSolutionSoFar, 1) +
               " de R$ 0,10. \n\t" +
               MinimizingMakeChangeFitnessFunction.getNumberOfCoinsAtGene(bestSolutionSoFar, 2) +
               " de R$ 0,05. \n\t" +
               MinimizingMakeChangeFitnessFunction.getNumberOfCoinsAtGene(bestSolutionSoFar, 3) +
               " de R$ 0,01. \n\t" +
               "Para um troco de " + MinimizingMakeChangeFitnessFunction.amountOfChange(bestSolutionSoFar) +
               " centavos em " +
               MinimizingMakeChangeFitnessFunction.getTotalNumberOfCoins(bestSolutionSoFar) + " moedas";

        JOptionPane.showMessageDialog(null, resposta);
    }

    /**
     * Grava o progresso em um arquivo - pode ser reutilizado para continuar
     * a execução
     */
    private static void saveStateInFile(Genotype population, String pathFile) throws Exception, IOException {
        DataTreeBuilder builder = DataTreeBuilder.getInstance();
        IDataCreators doc2 = builder.representGenotypeAsDocument(population);
        // create XML document from generated tree
        XMLDocumentBuilder docbuilder = new XMLDocumentBuilder();
        Document xmlDoc = (Document) docbuilder.buildDocument(doc2);
        XMLManager.writeFile(xmlDoc, new File(pathFile));
    }

    /**
     * Método Principal
     * @throws Exception
     *
     * Autores originais:
     * @author Neil Rotstan
     * @author Klaus Meffert
     * @since 1.0
     */
    public static void main(String[] args) throws Exception {
        int amount = 0;
        try {
            amount = Integer.parseInt(JOptionPane.showInputDialog("Valor monetário:"));
        } catch (NumberFormatException e) {
            System.out.println("Deve ser um valor inteiro válido.");
            System.exit(1);
        }

        if (amount < 1 || amount >= MinimizingMakeChangeFitnessFunction.MAX_BOUND) {
            System.out.println("O valor deve ser entre 1 e "
                    + (MinimizingMakeChangeFitnessFunction.MAX_BOUND - 1) + ".");
        } else {
            makeChangeForAmount(amount);
        }
    } //main

    /**
     * @param a_pop população a ser verificada
     * @return true se todos os cromossomos na população são únicos
     *
     * Autor original:
     * @author Klaus Meffert
     * @since 3.3.1
     */
    public static boolean uniqueChromosomes(Population a_pop) {
        /* verifica se todos os cromossomos são únicos */
        for (int i = 0; i < a_pop.size() - 1; i++) {
            IChromosome c = a_pop.getChromosome(i);
            for (int j = i + 1; j < a_pop.size(); j++) {
                IChromosome c2 = a_pop.getChromosome(j);
                if (c == c2) {
                    return false;
                }
            }
        }
        return true;
    }
}
