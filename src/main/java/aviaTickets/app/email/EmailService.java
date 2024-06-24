package aviaTickets.app.email;

import org.springframework.stereotype.Service;

import aviaTickets.app.customer.dto.ChangePwdDto;
import aviaTickets.app.email.entity.Email;
import aviaTickets.app.exception.ServerErrorException;

// import java.text.DateFormat;
// import java.time.LocalDate;
// import java.time.LocalDateTime;
// import java.util.Date;



@Service
public class EmailService implements EmailInteraction {
  private final String API_KEY = "test";
  // private String EMAIL_FROM = "";

  public EmailService() {};


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
    sendEmail(dto);
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
        throw new ServerErrorException("Can't get a html file.");
        
    }

  }

    // sendEmail -> send email use email API
    private void sendEmail(Email dto) {   
      String headers = String.format("'Content-type':'application/json', 'API_KEY': '%s'", API_KEY); // as example
      // api.send(this.EMAIL_FROM, headers, ctx);
    }

}
