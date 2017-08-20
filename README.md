# OSM_bus
An Application based on R and OSM (via Overpass turbo) to display bus lines in Dakar.

## Purpose
It's not easy to find the right bus line in Dakar. The mass transportation doesn't work like in France. Most of the time you need to ask the 'aprentis' to know where the bus went, and, moreover, you don't know if it goes through your targeted place.

Sine some years, [Dakar Dem Dickk](http://demdikk.com/) bus expend their working zones. If facilities are almost like in France on the battlefield, it's still hard to go out of our comfort zone.

So we decided to develop a simple application to know where we can find bus to go where we go

## Workflow

We based our work on [OSM](http://openstreetmap.org/) data, extract from the OSM database with [overpast turbo](http://overpass-turbo.eu/) with the following request :

    /*
    This has been generated by the overpass-turbo wizard.
    The original search was:
    “type=route & route=bus”
    */
    [out:json][timeout:25];
    // gather results
    (
      // query part for: “type=route and route=bus”
      relation["type"="route"]["route"="bus"]({{bbox}});
    );
    // print results
    out body;
    >;
    out skel qt;

We use [R](https://www.r-project.org/) with the [leaflet](https://rstudio.github.io/leaflet/) and [Shiny](https://rstudio.github.io/leaflet/) packages to build our webapp.
