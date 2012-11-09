package filmes.modelo;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

/**
 * Representa um Alelo
 * 
 * @author Pedro
 */

/*
 * { "Title": "True Grit", "Year": "1969", "Rated": "G", "Released":
 * "11 Jun 1969", "Runtime": "2 h 8 min", "Genre": "Adventure, Western, Drama",
 * "Director": "Henry Hathaway", "Writer": "Charles Portis, Marguerite Roberts",
 * "Actors": "John Wayne, Kim Darby, Glen Campbell, Jeremy Slate", "Plot":
 * "A drunken, hard-nosed U.S. Marshal and a Texas Ranger help a stubborn young woman track down her father's murderer in Indian territory."
 * , "Poster":
 * "http://ia.media-imdb.com/images/M/MV5BMTYwNTE3NDYzOV5BMl5BanBnXkFtZTcwNTU5MzY0MQ@@._V1_SX300.jpg"
 * , "imdbRating": "7.3", "imdbVotes": "20,783", "imdbID": "tt0065126",
 * "tomatoMeter": "89", "tomatoImage": "certified", "tomatoRating": "8",
 * "tomatoReviews": "47", "tomatoFresh": "42", "tomatoRotten": "5",
 * "tomatoConsensus": "No consensus yet.", "tomatoUserMeter": "81",
 * "tomatoUserRating": "3.8", "tomatoUserReviews": "23,875", "Response": "True"
 * }
 */
public class FilmeInfo implements Comparable<FilmeInfo> {
	private String Title;
	private String Year;
	private String Rated;
	private String Released;
	private String Runtime;
	private String Genre;
	private String Director;
	private String Writer;
	private String Actors;
	private String Plot;
	private String Poster;
	private String imdbRating;
	private String imdbVotes;
	private String imdbID;
	private String tomatoMeter;
	private String tomatoImage;
	private String tomatoRating;
	private String tomatoReviews;
	private String tomatoFresh;
	private String tomatoRotten;
	private String tomatoConsensus;
	private String tomatoUserMeter;
	private String tomatoUserRating;
	private String tomatoUserReviews;
	private String Response;

	public String getTitle() {
		return Title;
	}

	public int getYear() {
		return obterInt(Year);
	}

	public String getRated() {
		return Rated;
	}

	public Calendar getReleased() {
		return dateParser(Released);
	}

	/**
	 * @return O tempo de duracao do filme em minutos
	 */
	public int getRuntime() {
		return hourParser(Runtime);
	}

	public String getGenre() {
		return Genre;
	}

	public String getDirector() {
		return Director;
	}

	public String getWriter() {
		return Writer;
	}

	public String getActors() {
		return Actors;
	}

	public String getPlot() {
		return Plot;
	}

	public String getPoster() {
		return Poster;
	}

	public double getImdbRating() {
		return obterDouble(imdbRating);
	}

	public int getImdbVotes() {
		return obterInt(imdbVotes.replace(",", ""));
	}

	public void setImdbVotes(String imdbVotes) {
		this.imdbVotes = imdbVotes;

	}

	public String getImdbID() {
		return imdbID;
	}

	public void setImdbID(String imdbID) {
		this.imdbID = imdbID;
	}

	public int getTomatoMeter() {
		return obterInt(tomatoMeter);
	}

	public String getTomatoImage() {
		return tomatoImage;
	}

	public void setTomatoImage(String tomatoImage) {
		this.tomatoImage = tomatoImage;
	}

	public double getTomatoRating() {
		return obterDouble(tomatoRating);
	}

	public int getTomatoReviews() {
		return obterInt(tomatoReviews);
	}


	public int getTomatoFresh() {
		return obterInt(tomatoFresh);
	}


	public int getTomatoRotten() {
		return obterInt(tomatoRotten);
	}


	public String getTomatoConsensus() {
		return tomatoConsensus;
	}

	public void setTomatoConsensus(String tomatoConsensus) {
		this.tomatoConsensus = tomatoConsensus;
	}

	public int getTomatoUserMeter() {
		return obterInt(tomatoUserMeter);
	}


	public double getTomatoUserRating() {
		return obterDouble(tomatoUserRating);
	}

	public int getTomatoUserReviews() {
		return obterInt(tomatoUserReviews.replace(",", ""));
	}

	public String getResponse() {
		return Response;
	}

	public int compareTo(FilmeInfo arg0) {
		if (getImdbRating() == arg0.getImdbRating())
			return 0;
		return getImdbRating() < arg0.getImdbRating() ? 1 : -1;
	}

	private static int obterInt(String num) {
		try {
			return Integer.parseInt(num);
		} catch (Exception e) {
			return 0;
		}
	}

	private static double obterDouble(String num) {
		try {
			return Double.parseDouble(num);
		} catch (Exception e) {
			return 0;
		}
	}

	private static int hourParser(String hora) {
		String delims = "[ ]+";
		String[] tokens = hora.split(delims);
		return (Integer.parseInt(tokens[0]) * 60) + Integer.parseInt(tokens[2]);
	}

	private static Calendar dateParser(String data) {
		DateFormat dateParser = new SimpleDateFormat("dd MMMM yyyy");
		Date myDate = null;
		try {
			myDate = dateParser.parse(data);
		} catch (ParseException e) {
			e.printStackTrace();
		}
		Calendar calendar = Calendar.getInstance();
		calendar.setTime(myDate);
		return calendar;
	}

	@Override
	public String toString() {
		return "FilmeInfo [Actors=" + getActors() + ", Director=" + getDirector()
				+ ", Genre=" + getGenre() + ", Plot=" + getPlot() + ", Poster=" + getPoster()
				+ ", Rated=" + getRated() + ", Released=" + getReleased() + ", Response="
				+ getResponse() + ", Runtime=" + getRuntime() + ", Title=" + getTitle()
				+ ", Writer=" + getWriter() + ", Year=" + getYear() + ", imdbID="
				+ getImdbID() + ", imdbRating=" + getImdbID() + ", imdbVotes="
				+ getImdbVotes() + ", tomatoConsensus=" + getTomatoConsensus()
				+ ", tomatoFresh=" + getTomatoFresh() + ", tomatoImage="
				+ getTomatoImage() + ", tomatoMeter=" + getTomatoMeter()
				+ ", tomatoRating=" + getTomatoRating() + ", tomatoReviews="
				+ getTomatoReviews() + ", tomatoRotten=" + getTomatoRotten()
				+ ", tomatoUserMeter=" + getTomatoUserMeter()
				+ ", tomatoUserRating=" + getTomatoUserRating()
				+ ", tomatoUserReviews=" + getTomatoUserReviews() + "]";
	}

}
