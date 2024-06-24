package aviaTickets.app.auth;

import java.time.LocalDateTime;
import java.util.Optional;

import org.springframework.stereotype.Repository;

import aviaTickets.app.actions.ActionService;
import aviaTickets.app.actions.entity.ActionLog;
import aviaTickets.app.auth.dto.ForgotPwdDto;
import aviaTickets.app.auth.dto.SignInDto;
import aviaTickets.app.auth.dto.SignUpDto;
import aviaTickets.app.customer.CustomerRepository;
import aviaTickets.app.customer.entity.Customer;
import aviaTickets.app.email.EmailService;
import aviaTickets.util.HelperService;



abstract class Auth {
  // login 
  abstract void signIn(SignInDto dto);
  // registration
  abstract void signUp(SignUpDto dto);
  // send new password to user 
  abstract void forgotPassword(ForgotPwdDto dto);
  // activate user account by link
  abstract void activateByLink(String link);
}

@Repository
public class AuthRepository extends Auth {
  
  private final HelperService helperService;
  private final CustomerRepository customerRepository;
  private final EmailService emailService;
  private final ActionService actionService;

  public AuthRepository(
    HelperService helperService, EmailService emailService, 
    CustomerRepository customerRepository, ActionService actionService ) {
    this.helperService = helperService;
    this.emailService = emailService;
    this.customerRepository = customerRepository;
    this.actionService = actionService;
  }


  public void signIn(SignInDto dto) {

  }

  @Override
  public void signUp(SignUpDto dto) {
    customerRepository.createCustomer(dto.name(), dto.password(), dto.email());
    Optional<Customer> c = customerRepository.getCustomer(dto.email());
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
