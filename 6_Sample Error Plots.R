#################################################
## Final Summary ##
#################################################
###### Sample Error Plots ########
model_name <- c("glm1","glm2","nn1","nn2","nn3","rt1","rt2","rt3","glm3","glm4")
in_sample_error <- c(in_sample_error.glm1, in_sample_error.glm2, 
                     in_sample_error.nn1, in_sample_error.nn2,in_sample_error.nn3,
                     in_sample_error.rt1, in_sample_error.rt2[K], in_sample_error.rt3[K], 
                     in_sample_error.glm3, in_sample_error.glm4)*100
out_sample_error <- c(out_sample_error.glm1, out_sample_error.glm2, 
                      out_sample_error.nn1,out_sample_error.nn2,out_sample_error.nn3,
                      out_sample_error.rt1, out_sample_error.rt2[K], out_sample_error.rt3[K], 
                      out_sample_error.glm3, out_sample_error.glm4)*100
sample_error <- data.frame(cbind(model_name, in_sample_error, out_sample_error))
sample_error$in_sample_error <- as.double(in_sample_error)
sample_error$out_sample_error <- as.double(out_sample_error)
str(sample_error)

insampleplot <- sample_error %>% 
  ggplot(aes(model_name, in_sample_error)) + 
  geom_col(fill = c(rep("darkblue",2), rep("darkred",3), rep("darkgreen",3), rep("darkblue",2))) +
  scale_x_discrete(limit = c("glm1","glm2","nn1","nn2","nn3","rt1","rt2","rt3","glm3","glm4") ) + 
  coord_cartesian(ylim = c(8.0, 9.8)) + 
  theme(text = element_text(size = 15)) 

outsampleplot <- sample_error %>% 
  ggplot(aes(model_name, out_sample_error)) +
  geom_col(fill = c(rep("darkblue",2), rep("darkred",3), rep("darkgreen",3), rep("darkblue",2))) + 
  scale_x_discrete(limit = c("glm1","glm2","nn1","nn2","nn3","rt1","rt2","rt3","glm3","glm4") ) + 
  coord_cartesian(ylim = c(8.8, 9.3)) +
  theme(text = element_text(size = 15)) 

grid.arrange(insampleplot, outsampleplot)
