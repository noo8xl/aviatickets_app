package aviaTickets.app.email;

import org.springframework.jdbc.core.simple.JdbcClient;

import aviaTickets.app.customer.dto.ChangePwdDto;
import aviaTickets.app.email.entity.Email;

// import java.text.DateFormat;
// import java.time.LocalDate;
// import java.time.LocalDateTime;
// import java.util.Date;

abstract class Mailer {
  // -> send forgot password email 
  abstract void sendForgotPwdEmail(String email);
  // -> send new password to user email
  abstract void sendChangePwdEmail(ChangePwdDto dto);
  // -> send 2fa email to confirm customer action 
  abstract void sendTwoStepCode(String email);
  // -> send mail at sign up
  abstract void sendRegistrationEmail(String email);
} 



public class EmailService extends Mailer {
  private final String API_KEY = "";
  // private String EMAIL_FROM = "";

  private final JdbcClient jdbcClient;

  // private

  public EmailService(JdbcClient jdbcClient) {
    this.jdbcClient = jdbcClient;
  };


  public void sendForgotPwdEmail(String email) {

  }

  public void sendChangePwdEmail(ChangePwdDto dto) {
    // String ctx = getLetterContent("signUp");
    // Email dto = new Email(dto.userEmail(), dto.(), ctx, "");
    // this.sendEmail(dto);
  }

  public void sendTwoStepCode(String email) {

  }

  public void sendRegistrationEmail(String email) {
    // Date d = DateFormat.getDateTimeInstance(1, "LONG");
    String limk = ""; // -> link for the sign up confirmation
    String ctx = getLetterContent("signUp");
    Email dto = new Email(email, limk, ctx, "");
    this.sendEmail(dto);
  }
  
  // ### ----------------------------------------------------------------------------------- ###

  // getLetterContent -> get html file and convert it to string by name
  private String getLetterContent(String pageName) {
    String ctx = "test";

    switch (pageName) {
      case "signUp": // ==> html pages stores in resources -> static -> html -> email
      // ctx = fs.get("signUp.html");
        // try {
        // } catch (Exception e) {
        //   throw e;
        // }
        return ctx;
      default:
        return "";
        
    }

  }

  // sendEmail -> send email use email API
  private void sendEmail(Email dto) {   
    String headers = String.format("'Content-type':'application/json', 'API_KEY': '%s'", this.API_KEY); // as example
    // api.send(this.EMAIL_FROM, headers, ctx);
  }

}
