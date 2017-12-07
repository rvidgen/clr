# clR

clR package performs three kinds of analysis:

* Impact - calculates stats on impact publication has had in terms of citations. Calculates hindex for each article and each author.

* Stucture - builds force-directed graphs based on co-authorships of articles.

* Content - builds Latent Dirichlet Allocation model on article abstract text. 

## Prerequisites

The development packages of the following libraries need to be installed in order to compile clR's dependencies:

- libcurl
- openssl
- libssh2
- udunits2
- libxml2
- gsl

On CentOS, this requirement may be satisfied by executing the following command:

```# yum -y install libcurl-devel openssl-devel libssh2-devel udunits2-devel libxml2-devel gsl-devel```
