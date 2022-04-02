module BookingUpForBeauty

// The following line is needed to use the DateTime type
open System

let schedule (appointmentDateDescription: string): DateTime = 
    DateTime.Parse appointmentDateDescription

let hasPassed (appointmentDate: DateTime): bool = 
    DateTime.Now > appointmentDate

let isAfternoonAppointment (appointmentDate: DateTime): bool =
    appointmentDate.Hour >= 12 && appointmentDate.Hour < 18 
let description (appointmentDate: DateTime): string =
    let turnDescriptor = if appointmentDate.Hour >= 12 then "PM" else "AM"
    "You have an appointment on " + appointmentDate.ToString() + "." //+ " " + turnDescriptor

let anniversaryDate(): DateTime = 
    DateTime(DateTime.Now.Year,9,15,0,0,0)

let date = DateTime.Now
