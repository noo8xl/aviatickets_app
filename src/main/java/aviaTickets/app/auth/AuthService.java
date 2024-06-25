package aviaTickets.app.auth;

import java.time.LocalDateTime;
import java.util.Optional;

import org.springframework.stereotype.Service;

import aviaTickets.app.actions.ActionService;
import aviaTickets.app.actions.entity.ActionLog;
import aviaTickets.app.auth.dto.request.SignInDto;
import aviaTickets.app.auth.dto.request.SignUpDto;
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
  // private final JwtService jwtService;

  public AuthService(
    HelperService helperService, 
    EmailService emailService, 
    CustomerService customerService, 
    ActionService actionService
    // JwtService jwtService
    ) {
    this.helperService = helperService;
    this.emailService = emailService;
    this.customerService = customerService;
    this.actionService = actionService;
    // this.jwtService = jwtService;
  }


  public SignInResponse signIn(SignInDto dto) {
    Optional<Customer> customer = customerService.getCustomer(dto.email());
    if(customer.isEmpty()) throw new NotFoundException("User not found.");
    if(customer.get().password() != dto.password()) throw new BadRequestException("Whong email or password.");

    // Token t = jwtService.createToken(customer.get());
    // jwtService.save(t.get().refreshToken());

    return new SignInResponse(); // token pair, customer obj <-
  }


  public void signUp(SignUpDto dto) {
    customerService.createCustomer(dto.name(), dto.password(), dto.email());
    Optional<Customer> c = customerService.getCustomer(dto.email());
    emailService.sendRegistrationEmail(dto.email());
    setActionLog(c.get().id(), dto.email(), "User successfully signed up.");
  }

  public void forgotPassword(String email) {
    String pwd = helperService.generateUniqueString(16);
    Integer customerId = customerService.changePassword(email, pwd);
    emailService.sendForgotPwdEmail(email);
    setActionLog(customerId, email, "User password was changed.");
  }

  
  // ### ----------------------------------------------------------------------------------- ###

  private void setActionLog(Integer customerId, String email, String action) {
    ActionLog a = new ActionLog(
      null,
      email,
      LocalDateTime.now(),
      action,
      customerId
    );
    
    actionService.saveCustomerAction(a);
  }

}
