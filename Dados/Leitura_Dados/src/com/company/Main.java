package com.company;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Arrays;
import java.util.List;

public class Main
{
    private static final String path = "Dados/lista_adjacencias_paragens";

    public static void main(String[] args)
    {
        String extension = ".csv";
        List<Integer> carreiras = Arrays.asList(1, 2, 6, 7, 10, 11, 12, 13, 15, 23, 101, 102, 106, 108, 111, 112, 114,
                115, 116, 117, 119, 122, 125, 129, 158, 162, 171, 184, 201, 467, 468, 470, 471, 479, 714,
                748, 750, 751, 776);

        try
        {
            for (int i = 0; i < carreiras.size(); i++)
            {
                BufferedReader csvReader = new BufferedReader(new FileReader(path + "" +
                        carreiras.get(i) + extension));

                String row;
                System.out.println("\nCarreira -> " + carreiras.get(i) + "\n");
                while ((row = csvReader.readLine()) != null)
                {
                    String[] data = row.split(",");
                    // do something with the data           // FAZER PRINT PARA UM FICHEIRO DA ESTRUTURA QUE QUERO
                    System.out.println(data[0]);
                }

                csvReader.close();
            }
        }
        catch (Exception e)
        {
            System.out.println(e.toString());
        }
    }
}

