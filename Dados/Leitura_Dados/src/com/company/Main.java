package com.company;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Main
{
    private static final String path = "Dados/LAdj_";

    public static void main(String[] args)
    {
        String extension = ".csv";
        List<Integer> carreiras = Arrays.asList(1, 2, 6, 7, 10, 11, 12, 13, 15, 23, 101, 102, 106, 108, 111, 112, 114,
                115, 116, 117, 119, 122, 125, 129, 158, 162, 171, 184, 201, 467, 468, 470, 471, 479, 714,
                748, 750, 751, 776);

        List<Integer> paragensId = new ArrayList<>();

        try
        {
            FileWriter paragens = new FileWriter("../../paragens.pl");
            FileWriter viagens = new FileWriter("../../viagens.pl");

            String paragem = "";
            String viagem = "";

            paragens.write(":- dynamic paragem/10. \n");
            viagens.write(":- dynamic viagem/4. \n");

            for (int i = 0; i < carreiras.size(); i++)
            {
                String filePath = path + "" + carreiras.get(i) + extension;
                System.out.println(filePath);
                BufferedReader csvReader = new BufferedReader(new FileReader(filePath));

                String row;
                System.out.println("\nCarreira -> " + carreiras.get(i) + "\n");
                csvReader.readLine();

                Paragem pInicio = null;
                Paragem pFim = null;

                int iteracao = 0;

                while ((row = csvReader.readLine()) != null)
                {
                    iteracao++;
                    String[] data = row.split(";");
                    System.out.println("Data length -> " + data.length);

                    if (pInicio != null)
                    {
                        pFim = new Paragem(data[0], data[1], data[2], null);
                    }
                    else
                    {
                        pInicio = new Paragem(data[0], data[1], data[2], data[6]);
                    }

                    data[9] = data[9].replace("Á", "A");
                    data[1] = data[1].replace("N/A", "'N/A'");
                    data[2] = data[2].replace("N/A", "'N/A'");

                    // Para não haver paragens repetidas
                    if (!paragensId.contains(Integer.parseInt(data[0])))
                    {
                        paragensId.add(Integer.parseInt(data[0]));

                        // paragem(gid, latitude, longitude, estado de conservação, tipo de abrigo, abrigo com publicidade,
                        //         operadora, codigo de rua, nome da rua, freguesia).

                        paragem = "paragem(" + data[0] + ", " + data[1] + ", " + data[2] + ", '"
                                + data[3] + "', '" + data[4] + "', '" + data[5] + "', '" + data[6]
                                + "', " + data[8] + ", '" + data[9].replace("'", "")
                                + "', '" + data[10].replace("\"", "") + "').";

                        paragens.write(paragem + "\n");
                    }

                    pFim = new Paragem(data[0], data[1], data[2], data[6]);

                    String tempoViagem = pInicio.calculaTempo(pFim);

                    // viagem(carreira, paragemInicio, paragemFim, tempo de viagem).
                    viagem = "viagem(" + data[7] + ", " + pInicio.gid + ", " + pFim.gid
                            + ", " + tempoViagem + ").";

                    if (carreiras.get(i) == 6 || carreiras.get(i) == 7 || carreiras.get(i) == 10)
                    System.out.println(viagem);
                    //System.out.println(paragem);

                    pInicio = pFim;

                    if (iteracao > 1)
                        viagens.write(viagem + "\n");
                }
                csvReader.close();
            }

            paragens.close();
            viagens.close();
        }
        catch (Exception e)
        {
            System.out.println(e.toString());
        }
    }
}

