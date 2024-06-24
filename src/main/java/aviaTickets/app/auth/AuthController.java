package aviaTickets.app.auth;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import aviaTickets.app.auth.dto.ForgotPwdDto;
import aviaTickets.app.auth.dto.SignInDto;
import aviaTickets.app.auth.dto.SignUpDto;
import aviaTickets.app.customer.CustomerController;
import aviaTickets.app.customer.CustomerRepository;
import aviaTickets.app.exception.BadRequestException;
import aviaTickets.app.exception.NotFoundException;
import aviaTickets.app.exception.ServerErrorException;
import jakarta.validation.Valid;

@RestController
@Controller("/auth")
public class AuthController {
  private static final Logger log = LoggerFactory.getLogger(CustomerController.class);
  private final AuthRepository authRepository;
  private final CustomerRepository customerRepository;

  public AuthController(AuthRepository authRepository, CustomerRepository customerRepository) {
    this.authRepository = authRepository;
    this.customerRepository = customerRepository;
  }

  // signUp -> registration for the new customer 
  @ResponseStatus(HttpStatus.CREATED)
  @PostMapping("/signUp/")
  void signUp(@Valid @RequestBody SignUpDto dto) {
    try {
      Boolean isExist = customerRepository.isCustomerExists(dto.email());
      if(isExist) throw new BadRequestException("Bad request. User is already exists.");
      authRepository.signUp(dto);
    } catch (Exception e) {
      log.info("catch an error at '/auth/signUp/' \n->", e.getCause());
      throw new ServerErrorException("SignUp failed with " + e.getMessage());
    }
  }

  // signIn -> login area
  @ResponseStatus(HttpStatus.OK)
  @PostMapping("/signIn/")
  void signIn(@Valid @RequestBody SignInDto dto) {
    try {
      Boolean isExist = customerRepository.isCustomerExists(dto.email());
      if(!isExist) throw new NotFoundException("User not found.");
      // handle request here <-
      // bool 
      // if false -> notFoudException
    } catch (Exception e) {
      log.info("catch an error at '/auth/signIn/' \n->", e.getCause());
      throw new ServerErrorException("SignIn failed with " + e.getMessage());
    }
  }

  // forgotPassword -> send new pwd to user email
  @ResponseStatus(HttpStatus.ACCEPTED)
  @PatchMapping("/forgot-password/")
  void forgotPassword(@Valid @RequestBody ForgotPwdDto dto) {
    try {
      Boolean isExist = customerRepository.isCustomerExists(dto.email());
      if(!isExist) throw new NotFoundException("User not found.");
      // handle request here <-
    } catch (Exception e) {
      log.info("catch an error at '/auth/signIn/' \n->", e.getCause());
      throw new ServerErrorException("Send new password failed with " + e.getMessage());
    }
  }

  // activateAccount -> activate account by link
  @ResponseStatus(HttpStatus.ACCEPTED)
  @PatchMapping("/activate/{link}")
  void activateAccount(@PathVariable String link) {
    try {
      // handle request here <-
    } catch (Exception e) {
      log.info("catch an error at '/auth/signIn/' \n->", e.getCause());
      throw new ServerErrorException("Account activation failed with " + e.getMessage());
    }
  }



}
