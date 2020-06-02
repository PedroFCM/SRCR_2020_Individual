package com.company;

import java.util.ArrayList;
import java.util.List;

public class Viagem
{
    public String gidInicio;
    public String gidFim;

    public String tempo;

    public List<String> carreiras = new ArrayList<>();

    public Viagem()
    {
        this.carreiras = new ArrayList<>();
    }

    public String hash()
    {
        return gidInicio + ", " + gidFim;
    }

    @Override
    public String toString()
    {
        String carreirasS = "";

        for (int i = 0; i < carreiras.size(); i++)
        {
            if (i < carreiras.size() -1)
                carreirasS += carreiras.get(i) + ", ";
            else
                carreirasS += carreiras.get(i);
        }

        return "viagem([" + carreirasS + "], " +
                gidInicio + ", "
                + gidFim + ", " + tempo + ").";
    }
}
