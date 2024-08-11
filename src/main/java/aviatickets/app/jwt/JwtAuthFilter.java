package aviatickets.app.jwt;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.jetbrains.annotations.NotNull;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@RequiredArgsConstructor
@Component
public class JwtAuthFilter extends OncePerRequestFilter {

	private final JwtInterface jwtService;
	private final UserDetailsService userDetailsService;

	@Override
	protected void doFilterInternal(
			HttpServletRequest request,
			@NotNull HttpServletResponse response,
			@NotNull FilterChain filterChain
		) throws ServletException, IOException {

			String authHeader = request.getHeader("Authorization");
			String jwt;
			String customerEmail;

			if (authHeader == null ) {
				filterChain.doFilter(request, response);
				return;
			}
			if(Boolean.FALSE.equals(authHeader.startsWith("Bearer "))) {
				filterChain.doFilter(request, response);
				return;
			}

		try {
			jwt = authHeader.substring(7);
			customerEmail = this.jwtService.extractCustomerEmail(jwt);

			Authentication authentication = SecurityContextHolder.getContext().getAuthentication();

			if(customerEmail != null && authentication == null) {
				UserDetails userDetails = this.userDetailsService.loadUserByUsername(customerEmail);
				Boolean isTokenValid = this.jwtService.isTokenValid(jwt, userDetails);

				// from ->
				if(Boolean.TRUE.equals(isTokenValid)) {
					UsernamePasswordAuthenticationToken authToken = new UsernamePasswordAuthenticationToken(
						userDetails,
						null,
						userDetails.getAuthorities()
					);
					authToken.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
					SecurityContextHolder.getContext().setAuthentication(authToken);
				}
			}
			System.out.println("auth ctx -> "+ SecurityContextHolder.getContext().getAuthentication());
			filterChain.doFilter(request, response);
		} catch (Exception e) {
			throw new ServletException(e);
		}
	}
}
