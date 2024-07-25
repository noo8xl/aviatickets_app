package aviatickets.app.auth.dto.response;

import aviatickets.app.customer.entity.Customer;

public record SignInResponse(
  // Token t,
   Customer c
) {}
