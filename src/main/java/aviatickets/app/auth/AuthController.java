package aviatickets.app.auth;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import aviatickets.app.auth.dto.request.SignInDto;
import aviatickets.app.auth.dto.request.SignUpDto;
import aviatickets.app.auth.dto.response.SignInResponse;
import aviatickets.app.customer.CustomerService;
import aviatickets.app.exception.BadRequestException;
import aviatickets.app.exception.NotFoundException;
import aviatickets.app.exception.ServerErrorException;
import jakarta.validation.Valid;

@RestController
@RequestMapping("/auth")
public class AuthController {
  private final AuthService authService;
  private final CustomerService customerService;

  public AuthController(AuthService authService, CustomerService customerService) {
    this.authService = authService;
    this.customerService = customerService;
  }

  // signUp -> registration for the new customer
  @ResponseStatus(HttpStatus.CREATED)
  @PostMapping("/signUp/")
  void signUp(@Valid @RequestBody SignUpDto dto) {
    try {
      Boolean isExist = customerService.isCustomerExists(dto.email());
      if (Boolean.TRUE.equals(isExist))
        throw new BadRequestException("Bad request. User is already exists.");
      authService.signUp(dto);
    } catch (Exception e) {
      throw new ServerErrorException("SignUp failed with " + e.getMessage());
    }
  }

  // signIn -> login area
  @ResponseStatus(HttpStatus.OK)
  @PostMapping("/signIn/")
  ResponseEntity<SignInResponse> signIn(@Valid @RequestBody SignInDto dto) {
    try {
      SignInResponse resp = authService.signIn(dto);
      return ResponseEntity.ok(resp);
    } catch (Exception e) {
      throw new ServerErrorException("SignIn failed with " + e.getMessage());
    }
  }

  // forgotPassword -> send new pwd to user email
  @ResponseStatus(HttpStatus.ACCEPTED)
  @PatchMapping("/forgot-password/{email}")
  void forgotPassword(@PathVariable String email) {
    try {
      Boolean isExist = customerService.isCustomerExists(email);
      if (Boolean.FALSE.equals(isExist))
        throw new NotFoundException("User not found.");
      authService.forgotPassword(email);
    } catch (Exception e) {
      throw new ServerErrorException("Send new password failed with " + e.getMessage());
    }
  }

}
