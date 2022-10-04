import Z

-- https://softwaremill.com/structuring-zio-2-applications/


structure Connection := (id: String)
structure Car := (make model licencePlate: String)
structure LicensePlateExistsError := (licensePlate: String)


open Console (consoleLive)


structure ConnectionPool where
  r : IO.Ref (List Connection)

  obtain : IO Connection := 
    r.modifyGet fun
      | h :: t => (h, t)
      | [] => sorry

  release (c: Connection) : IO Unit := 
    r.modify (c :: .) 

  close : IO Unit := do
    let conns <- r.modifyGet fun c => (c, [])
    for con in conns do
      IO.println s!"Closing: {con.id}"
    


structure CarRepository where
  licenseExists (licencePlate: String): Z Connection Empty Bool := do
    let _ <- Z.service Connection
    let starts := licencePlate.startsWith "WN"
    consoleLive.printLine s!"Checking if license plate exists: {licencePlate}"
    return starts
  
  insert (car: Car) : Z Connection Empty Unit := do
    let _ <- Z.service Connection
    consoleLive.printLine s!"Inserting car: {car.licencePlate}"


structure CarService where
  careRepository: CarRepository


structure CarApi where
  carService: CarService