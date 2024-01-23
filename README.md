# Interactive Visualization of Nonlinear Dimensionality Reduction Methods for Complex Data

When working with complex data like functional or image data the challenge of effective visualization can be challenging. 
Linear or nonlinear dimensionality reduction methods such as MDS, t-SNE or UMAP are popular visualization methods for high-dimensional data, as t
he resulting embeddings then allow for scatterplot visualizations.
As part of my master's thesis I developed a pair of interactive R Shiny apps tailored to embeddings of both functional and image data.
They let users interact with individual or selected groups of data points, visualize the represented data, and categorize them by additional information. 
They also allow for comparison of different embedding dimensions and methods. 

## Description

The three different visualization tabs offer a variety of options to display the embedding scatterplots and the corresponding curves. 
The first tab, *Viz: Functional data*, (or *Viz: Image data*)  plots a single plot of two chosen embedding dimensions, and offers a wider variety of plotting options.
The tab *Viz: Embeddings}* plots all dimensions of all given embedding methods against each other. 
This gives an overview of the results of all the given embeddings. The tab *Viz: Matrix* plots all dimensions of a chosen embedding method in a scatter
plot matrix for intuitive visualization of embedding dimensionalities that are higher than 2D.
The tab *Data* shows the currently loaded dataset and contains the option to upload another dataset, 
while the tab *About* consists of a general description as well as background and contact information.
Each tab offers a description of the functionalities by clicking on 'More information'.

In each of the tabs, the actual data can be visualized by selecting points in the scatterplot.

## Getting Started

The functional data app can be accessed through the provided code or online at
https://jukaje.shinyapps.io/Embed_it/

The image data app at
https://jukaje.shinyapps.io/Embed_it2/

Each of the apps comes with a pre-loaded example dataset. Further datasets can be found in **Datasets**.

## License

This project is licensed under the GNU General Public License v3.0
