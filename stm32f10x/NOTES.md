ajhc ver was showing no signs of life
tried other C demos like stm_p103_demos collection. nothing, with flashing warnings/errors.
Then STM32F103VHB6_RevZ_Demo1_Lanchon_20080210.zip worked; used include stm.._lib instead as include file and SetBit() funcs
all the init code in the above commented out made no diff, all we needed was SetBit()
now gonna make ajhc use SetBit() via ffi since they seem to set something important
