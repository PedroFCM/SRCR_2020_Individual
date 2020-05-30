package com.company;

import java.util.Locale;

public class Paragem
{
    public String gid;
    public String latitude;
    public String longitude;

    public String operadoraViagem;

    public Paragem(String gid, String latitude, String longitude, String operadoraViagem)
    {
        this.gid = gid;
        this.latitude = latitude;
        this.longitude = longitude;
        this.operadoraViagem = operadoraViagem;
    }

    public String calculaTempo(Paragem pFIm)
    {
        if (this.latitude.equals("'N/A'") || pFIm.latitude.equals("'N/A'")
            || this.longitude.equals("'N/A'") || pFIm.longitude.equals("'N/A'"))
        {
            return "'N/A'";
        }

        float latI = Float.parseFloat(this.latitude);
        float latF = Float.parseFloat(pFIm.latitude);
        float longI = Float.parseFloat(this.longitude);
        float longF = Float.parseFloat(pFIm.longitude);

        // Assumindo que a latitude e longitude estão em metros...

        double deltaLat = Math.pow((latF - latI), 2);  // em metros
        double deltaLong = Math.pow((longF - longI), 2);  // em metros

        // Resultado em minutos
        return String.format(Locale.US, "%.2f", (Math.sqrt(deltaLat + deltaLong))/1000);
    }
}
