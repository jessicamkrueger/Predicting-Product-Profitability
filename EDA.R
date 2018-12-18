ggplot(readyData, aes(x=Depth, y=Volume)) +
  geom_point()

ggplot(readyData, aes(x=X4Stars, y=Volume)) +
  geom_point()

#Looking at X5Stars vs. Volume for each product type
ggplot(Team_Data3, aes(x=X5Stars, y=Volume)) +
  geom_point() +
  facet_wrap(~Product_type, scales = "free")

#Looking at Depth vs. Volume for each product type
Team_Data3 %>% 
  subset(Product_type == "Laptop") %>%
  ggplot(aes(x=Depth, y=Volume)) +
  geom_point() +
  facet_wrap(~Product_type)

Team_Data3 %>% 
  subset(Product_type == "PC") %>%
  ggplot(aes(x=Depth, y=Volume)) +
  geom_point() +
  facet_wrap(~Product_type)

Team_Data3 %>% 
  subset(Product_type == "Netbook") %>%
  ggplot(aes(x=Depth, y=Volume)) +
  geom_point() +
  facet_wrap(~Product_type)

Team_Data3 %>% 
  subset(Product_type == "Smartphone") %>%
  ggplot(aes(x=Depth, y=Volume)) +
  geom_point() +
  facet_wrap(~Product_type)

#is there a relationship with Gender?
ggplot(Team_Data3, aes(x=Gender, y=Volume)) +
  geom_col() +
  facet_wrap(~Product_type, scales = "free")

ggplot(Team_Data3, aes(x=Gender, y=Volume)) +
  geom_col()
#yes for some products
#0=Female, 1=Male

#is there a relationship with Age?
ggplot(Team_Data3, aes(x=Age, y=Volume)) +
  geom_col() +
  facet_wrap(~Product_type, scales = "free")

ggplot(Team_Data3, aes(x=Age, y=Volume)) +
  geom_col()

#is there a relationship with In_store?
ggplot(Team_Data3, aes(x=In_store, y=Volume)) +
  geom_col() +
  facet_wrap(~Product_type, scales = "free")

#is there a relationship with Environmental Impact?
ggplot(Team_Data3, aes(x=Environment_Impact, y=Volume)) +
  geom_col() +
  facet_wrap(~Product_type, scales = "free")

#is there a relationship with Durability
ggplot(Team_Data3, aes(x=Durability_standard, y=Volume)) +
  geom_line() +
  facet_wrap(~Product_type, scales = "free")

#is there a relationship with relative price
ggplot(Team_Data3, aes(x=Relative_Price, y=Volume)) +
  geom_line() +
  facet_wrap(~Product_type, scales = "free")

#is there a relationship with Weight
ggplot(Team_Data3, aes(x=Weight, y=Volume)) +
  geom_line() +
  facet_wrap(~Product_type, scales = "free")

#is there a relationship with Height
ggplot(Team_Data3, aes(x=Height, y=Volume)) +
  geom_line() +
  facet_wrap(~Product_type, scales = "free")

#is there a relationship with Width
ggplot(Team_Data3, aes(x=Width, y=Volume)) +
  geom_point() +
  facet_wrap(~Product_type, scales = "free")

#is there a relationship with Depth
ggplot(Team_Data3, aes(x=Depth, y=Volume)) +
  geom_line() +
  facet_wrap(~Product_type, scales = "free")