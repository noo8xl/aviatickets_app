package aviaTickets.app.auth;

import java.time.LocalDateTime;
import java.util.Optional;

import org.springframework.stereotype.Service;

import aviaTickets.app.actions.ActionService;
import aviaTickets.app.actions.entity.ActionLog;
import aviaTickets.app.auth.dto.ForgotPwdDto;
import aviaTickets.app.auth.dto.SignInDto;
import aviaTickets.app.auth.dto.SignUpDto;
import aviaTickets.app.auth.dto.response.SignInResponse;
import aviaTickets.app.customer.CustomerService;
import aviaTickets.app.customer.entity.Customer;
import aviaTickets.app.email.EmailService;
import aviaTickets.app.exception.BadRequestException;
import aviaTickets.app.exception.NotFoundException;
import aviaTickets.util.HelperService;


@Service
public class AuthService implements AuthInteraction {
  
  private final HelperService helperService;
  private final CustomerService customerService;
  private final EmailService emailService;
  private final ActionService actionService;
  // private final TokenService tokenService;

  public AuthService(
    HelperService helperService, 
    EmailService emailService, 
    CustomerService customerService, 
    ActionService actionService
    // TokenService tokenService
    ) {
    this.helperService = helperService;
    this.emailService = emailService;
    this.customerService = customerService;
    this.actionService = actionService;
    // this.tokenService = tokenService;
  }


  public SignInResponse signIn(SignInDto dto) {
    // String email,
    // String password,
    // Boolean twoStep,
    // String code

    Optional<Customer> customer = customerService.getCustomer(dto.email());
    if(customer.isEmpty()) throw new NotFoundException("User not found.");
    if(customer.get().password() != dto.password()) throw new BadRequestException("Whong email or password.");

    // Token t = tokenService.createToken(customer.get());
    // tokenService.save(t.get().refreshToken());

    return new SignInResponse(); // token pair, customer obj <-
  }


  public void signUp(SignUpDto dto) {
    customerService.createCustomer(dto.name(), dto.password(), dto.email());
    Optional<Customer> c = customerService.getCustomer(dto.email());
    emailService.sendRegistrationEmail(dto.email());
    ActionLog a = new ActionLog(
      null,
      dto.email(),
      LocalDateTime.now(),
      "User successfully signed up.",
      c.get().id()
    );
    actionService.saveCustomerAction(a);
  }

  public void forgotPassword(ForgotPwdDto dto) {
    String pwd = helperService.generateUniqueString(16);
    System.out.print("new pwd -> ");
    System.out.println(pwd);

  }

  public void activateByLink(String link) {


  }

  
  // ### ----------------------------------------------------------------------------------- ###


}
