# Financing Wind Power Projects
This is the final project from my specialization in finance

Liability of WindFarms in Brazil

This project analyses the financial liability of windfarms,largerly
affected by Nacional Bank of Development's(BNDES) higher taxes for
infrastructure projects funding.In the past, the renewables sector in Brazil
was highly dependent on public banks funding,but now there is a need
to evaluate other kinds of funding.The algorithm presented here calculates
the net value for the desired proportion of hedge,debentures and
BNDES funding.The user can play with this three variables(and a set of
other variables described in the code),in order to achieve
the highest net value.

Usage:
fc.us(energy price R$/MWh,energy sold per year(x 1000 MWh),
total investment(x R$1000),%hedge,%bndes funding,%debentures)
Returns the Net Value in R$

Example:
fc.usina(155,116,175000,20,80,0)
8420386 
