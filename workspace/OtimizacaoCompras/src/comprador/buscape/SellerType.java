//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vJAXB 2.1.10 in JDK 6 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2012.04.13 at 10:56:59 AM BRT 
//


package comprador.buscape;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for SellerType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="SellerType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="sellerName" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="thumbnail" type="{urn:buscape}ThumbnailType"/>
 *         &lt;element name="links" type="{urn:buscape}LinksType"/>
 *         &lt;element name="extra" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="contacts" type="{urn:buscape}ContactsType"/>
 *         &lt;element name="rating" type="{urn:buscape}RatingType"/>
 *         &lt;element name="coupon" type="{urn:buscape}CouponType"/>
 *         &lt;element name="addresses" type="{urn:buscape}AddressesType"/>
 *       &lt;/sequence>
 *       &lt;attribute name="id" use="required" type="{http://www.w3.org/2001/XMLSchema}int" />
 *       &lt;attribute name="isTrustedStore" use="required" type="{http://www.w3.org/2001/XMLSchema}boolean" />
 *       &lt;attribute name="pagamentoDigital" use="required" type="{http://www.w3.org/2001/XMLSchema}boolean" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SellerType", propOrder = {
    "sellerName",
    "thumbnail",
    "links",
    "extra",
    "contacts",
    "rating",
    "coupon",
    "addresses"
})
public class SellerType
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(required = true)
    protected String sellerName;
    @XmlElement(required = true)
    protected ThumbnailType thumbnail;
    @XmlElement(required = true)
    protected LinksType links;
    @XmlElement(required = true)
    protected String extra;
    @XmlElement(required = true)
    protected ContactsType contacts;
    @XmlElement(required = true)
    protected RatingType rating;
    @XmlElement(required = true)
    protected CouponType coupon;
    @XmlElement(required = true)
    protected AddressesType addresses;
    @XmlAttribute(required = true)
    protected int id;
    @XmlAttribute(required = true)
    protected boolean isTrustedStore;
    @XmlAttribute(required = true)
    protected boolean pagamentoDigital;

    /**
     * Gets the value of the sellerName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSellerName() {
        return sellerName;
    }

    /**
     * Sets the value of the sellerName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSellerName(String value) {
        this.sellerName = value;
    }

    /**
     * Gets the value of the thumbnail property.
     * 
     * @return
     *     possible object is
     *     {@link ThumbnailType }
     *     
     */
    public ThumbnailType getThumbnail() {
        return thumbnail;
    }

    /**
     * Sets the value of the thumbnail property.
     * 
     * @param value
     *     allowed object is
     *     {@link ThumbnailType }
     *     
     */
    public void setThumbnail(ThumbnailType value) {
        this.thumbnail = value;
    }

    /**
     * Gets the value of the links property.
     * 
     * @return
     *     possible object is
     *     {@link LinksType }
     *     
     */
    public LinksType getLinks() {
        return links;
    }

    /**
     * Sets the value of the links property.
     * 
     * @param value
     *     allowed object is
     *     {@link LinksType }
     *     
     */
    public void setLinks(LinksType value) {
        this.links = value;
    }

    /**
     * Gets the value of the extra property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getExtra() {
        return extra;
    }

    /**
     * Sets the value of the extra property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setExtra(String value) {
        this.extra = value;
    }

    /**
     * Gets the value of the contacts property.
     * 
     * @return
     *     possible object is
     *     {@link ContactsType }
     *     
     */
    public ContactsType getContacts() {
        return contacts;
    }

    /**
     * Sets the value of the contacts property.
     * 
     * @param value
     *     allowed object is
     *     {@link ContactsType }
     *     
     */
    public void setContacts(ContactsType value) {
        this.contacts = value;
    }

    /**
     * Gets the value of the rating property.
     * 
     * @return
     *     possible object is
     *     {@link RatingType }
     *     
     */
    public RatingType getRating() {
        return rating;
    }

    /**
     * Sets the value of the rating property.
     * 
     * @param value
     *     allowed object is
     *     {@link RatingType }
     *     
     */
    public void setRating(RatingType value) {
        this.rating = value;
    }

    /**
     * Gets the value of the coupon property.
     * 
     * @return
     *     possible object is
     *     {@link CouponType }
     *     
     */
    public CouponType getCoupon() {
        return coupon;
    }

    /**
     * Sets the value of the coupon property.
     * 
     * @param value
     *     allowed object is
     *     {@link CouponType }
     *     
     */
    public void setCoupon(CouponType value) {
        this.coupon = value;
    }

    /**
     * Gets the value of the addresses property.
     * 
     * @return
     *     possible object is
     *     {@link AddressesType }
     *     
     */
    public AddressesType getAddresses() {
        return addresses;
    }

    /**
     * Sets the value of the addresses property.
     * 
     * @param value
     *     allowed object is
     *     {@link AddressesType }
     *     
     */
    public void setAddresses(AddressesType value) {
        this.addresses = value;
    }

    /**
     * Gets the value of the id property.
     * 
     */
    public int getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     */
    public void setId(int value) {
        this.id = value;
    }

    /**
     * Gets the value of the isTrustedStore property.
     * 
     */
    public boolean isIsTrustedStore() {
        return isTrustedStore;
    }

    /**
     * Sets the value of the isTrustedStore property.
     * 
     */
    public void setIsTrustedStore(boolean value) {
        this.isTrustedStore = value;
    }

    /**
     * Gets the value of the pagamentoDigital property.
     * 
     */
    public boolean isPagamentoDigital() {
        return pagamentoDigital;
    }

    /**
     * Sets the value of the pagamentoDigital property.
     * 
     */
    public void setPagamentoDigital(boolean value) {
        this.pagamentoDigital = value;
    }

}
