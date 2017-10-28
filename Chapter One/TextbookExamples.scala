/*
* Example of a Scala program with side effects 
*/ 

class Cafe {
    def buyCoffee(cc : CreditCard) : Coffee = {
        val cup = new Coffee()
        cc.charge(cup.price) /* Example of a side effect. 
        cup
    }
}

/*
* Note that this isn't a huge improvement!  Although our code is now easier to 
* test (though still requires a heafty interface), it isn't very reusable
*/
class Cafe {
    def buyCoffee(cc : CreditCard, p : Payment) : Coffee = {
        val cup = new Coffee()
        p.charge(cc, cup.price)
        cup
    }
}

/*
* Now we try and improve how we purchase coffee to remove side effects 
*/
// A case class has one primary constructor whose arguement list comes after
// the calss name (here, Charge). The parameters in this list become public, 
// unmodifiable (immutable) fields of the class and can be accessed using the usual object-oriented dot notation,
// as in other.cc
case class Charge(cc : CreditCard , amount : Double){ 
    def combine( other : Charge) : Charge = 
        if (cc == other.cc)
            Charge(cc, amount+other.amount)
        else 
            throw new Exception("Can't combine charges to different cards")
}

/*
* Our flashy, brand new Cafe function! Now with no mutability and side effects!
*/ 
class Cafe {
    def buyCoffee(cc: CreditCard) : (Coffee, Charge) = ....

    def buyCoffees(cc : CreditCard, n : Int) (List[Coffee], Charge) = {
        val purchases : List[(Coffee, Charge)] = List.fill(n) (buyCoffee(cc))
        val (coffees, charges) = purchases.unzip
        (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
    }
}

// Final function for reducing all purchases with the possability of 
// multiple cards being used 
def coalesce(charges: List[Charge]): List[Charge] = 
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
