R CMD /usr/lib/R/library/Rserve/libs/Rserve --RS-conf /root/rs.conf --no-save
/usr/lib/R/library/Rserve/libs/forward -p 8080 -R /var/run/Rscript -r /var/www
