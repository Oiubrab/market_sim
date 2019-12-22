!subroutine section:
!need to add format strings to all the print statements
subroutine buy_sell(buyer,buyer_size,buyer_number,energy,item_choice_k,item_choice_i,exchange_ammount) !the market form has k then i
implicit none
integer,intent(in) :: item_choice_k,item_choice_i,exchange_ammount,buyer_size,buyer_number
integer,dimension(buyer_size) :: buyer !the amount the buyer has in savings
integer,intent(inout),dimension(10,10) :: energy

energy(item_choice_k,item_choice_i)=energy(item_choice_k,item_choice_i)-exchange_ammount !purchasing is thus positive exchange amount and selling is negative
buyer(buyer_number)=buyer(buyer_number)+exchange_ammount

end subroutine buy_sell

subroutine print_market(energy,k_size,i_size,buyer,buyer_size)
implicit none
integer,intent(in) :: k_size,i_size,buyer_size
integer,dimension(buyer_size) :: buyer
integer,dimension(k_size,i_size),intent(in) :: energy
integer :: k,i,l

do k=1,k_size
    do i=1,i_size
        print'(A21,I2,A1,I2,A4,I3)',"the value of energy(",k,',',i,") is", energy(k,i)
    enddo
enddo

print*," "

l=1
print*,"Buyer",l,"has",buyer(l),"energy units (user)"

do l=2,buyer_size
    print*,"Buyer",l,"has",buyer(l),"energy units"
enddo

end subroutine print_market

subroutine user_buyer_interface(energy,k_size,i_size,buyer,buyer_size) !the user buyer is always the first element in the buyer list
integer,intent(in) :: k_size,i_size,buyer_size
integer,dimension(buyer_size) :: buyer
integer,dimension(k_size,i_size),intent(inout) :: energy
integer :: period,item_choice_k,item_choice_i,exchange_ammount,exchange_error,buyer1
character(len=3) :: choice_buy_sell

buyer1=buyer(1)

print*,"do you want to buy/sell this round?"
read (*,*) choice_buy_sell
if (choice_buy_sell/='yes' .or. choice_buy_sell/='no') then !stupid users, choose yes or no
    do while (choice_buy_sell/='yes' .and. choice_buy_sell/='no') !beat them into submission        
        print*,"incorrect choice, try again."
        read (*,*) choice_buy_sell
    enddo
endif
if (choice_buy_sell=="yes") then !start buy/sell procedure
    print'(A78,I0,A33,I0)',"where would you like to buy/sell from/into (enter first number between 1 and ", &
    size(energy(:,1)),"and second number between 1 and",size(energy(1,:)),")"
    read (*,*) item_choice_k,item_choice_i !finds the required item in the list and records it
    do while ((1>item_choice_k .or. item_choice_k>k_size) .or. (1>item_choice_i .or. item_choice_i>i_size)) !must enter available item
        print*,"incorrect choice, try again."
        read (*,*) item_choice_k,item_choice_i
    enddo
    print*,"how much do you want to buy/sell (positive for buy and negative for sell)?"
    read (*,'(i10)',iostat=exchange_error) exchange_ammount !puts error associated with non integr in exchange_error, where 0 is non-error
    do while (exchange_error/=0)
        print*,"incorrect choice, try again."
        read (*,'(i10)',iostat=exchange_error) exchange_ammount
    enddo
    call buy_sell(buyer,buyer_size,1,energy,item_choice_k,item_choice_i,exchange_ammount)
    print*,"now let the other buyers have their turn"
else if (choice_buy_sell=="no") then
    print*,"now let the other buyers have their turn"
endif
call print_market(energy,k_size,i_size,buyer,buyer_size)

end subroutine user_buyer_interface

subroutine automated_buyers(energy,k_size,i_size,buyer,buyer_size)
integer,intent(in) :: k_size,i_size,buyer_size
integer,dimension(k_size,i_size) :: energy
integer,dimension(buyer_size) :: buyer
real :: k_real,i_real,buy_real !real variables to put in random generator
integer :: h,ran_k,ran_i,ran_buy !integers taken from real random values previous

do h=2,buyer_size
    call random_number(k_real)
    call random_number(i_real)
    call random_number(buy_real)
    ran_k = Int( k_real * 10 ) + 1
    ran_i = Int( i_real * 10 ) + 1
    ran_buy=-5+floor(buy_real*11)
    call buy_sell(buyer,buyer_size,h,energy,ran_k,ran_i,ran_buy)
enddo

end subroutine automated_buyers

!end subroutines





program market
implicit none

integer,dimension(10,10) :: energy
integer,dimension(10) :: buyer
integer :: period,k,i,l,item_choice_k,item_choice_i,exchange_ammount,exchange_error
integer :: k_size=size(energy(:,1)),i_size=size(energy(1,:)),buyer_size=size(buyer) !setup the size variables in the definition

do k=1,k_size
    do i=1,i_size
        energy(k,i)= 1 !setting up an array representing a market, putting in initial values 
    enddo
enddo

do l=1,buyer_size
    buyer(l)=0
enddo

do period=1,10
    call print_market(energy,k_size,i_size,buyer,buyer_size)
    call user_buyer_interface(energy,k_size,i_size,buyer,buyer_size)
    call automated_buyers(energy,k_size,i_size,buyer,buyer_size)
enddo

end program market
