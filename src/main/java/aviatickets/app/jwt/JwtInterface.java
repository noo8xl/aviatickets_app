package aviatickets.app.jwt;

import io.jsonwebtoken.Claims;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.function.Function;

public interface JwtInterface {

	String extractCustomerEmail(String token);
	String generateToken(UserDetails userDetails);
	Boolean isTokenValid(String token, UserDetails userDetails);
	<T> T getClaim(String token, Function<Claims, T> claimsResolver);

}
