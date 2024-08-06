package aviatickets.app.email;

import org.springframework.stereotype.Service;

import aviatickets.app.customer.dto.ChangePwdDto;
import aviatickets.app.email.entity.Email;
import aviatickets.app.exception.ServerErrorException;

// import java.text.DateFormat;
// import java.time.LocalDate;
// import java.time.LocalDateTime;
// import java.util.Date;

@Service
public class EmailService implements EmailInteraction {

  private static String apiKey = "test";
  // private String EMAIL_FROM = "";

	public EmailService() {}

	@Override
  public void sendForgotPwdEmail(String email, String pwd) {
		System.out.println("data is -> "+ email + " " + pwd);
    // TODO
  }

	@Override
  public void sendChangePwdEmail(ChangePwdDto dto) {
    // String ctx = getLetterContent("signUp");
    // Email dto = new Email(dto.userEmail(), dto.(), ctx, "");
    // this.sendEmail(dto);
  }

	@Override
  public void sendTwoStepCode(String email) {

  }

	@Override
  public void sendRegistrationEmail(String email) {
    // Date d = DateFormat.getDateTimeInstance(1, "LONG");
    String link = ""; // -> link for the sign-up confirmation
    String ctx = getLetterContent("signUp");
    Email dto = new Email(email, link, ctx, "");
    sendEmail(dto);
  }

	@Override
	public void sendNewPurchaseEmail(String email) {

	}

  // -----------------------------------------------------------------------------------

  // getLetterContent -> get html file and convert it to string by name
  private String getLetterContent(String pageName) {
    String ctx = "test";

    switch (pageName) {
      case "signUp": // ==> html pages stores in resources -> static -> html -> email
        // ctx = fs.get("signUp.html");
        // try {
        // } catch (Exception e) {
        // throw e;
        // }
        return ctx;
      case "twoStep":
        return ctx;
      case "forgotPwd":
        return ctx;
      case "changePwd":
        return ctx;
			case "purchase":
				return ctx;
      default:
        throw new ServerErrorException("Can't get html file.");

    }

  }

  // sendEmail -> send email use email API
  private void sendEmail(Email dto) {
    String headers = String.format("'Content-type':'application/json', 'API_KEY': {}", this.apiKey); // as example
    // api.send(this.EMAIL_FROM, headers, ctx);
  }

}
