package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.i18n.{MessagesApi, I18nSupport}
import scala.collection.mutable.{Map, SynchronizedMap, HashMap}

case class Contato(var id: Long, nome: String, email: String, telefone: String)

@Singleton
class HomeController @Inject() (val messagesApi: MessagesApi) extends Controller with I18nSupport {


    def login(name: String) = Action { request =>
    Redirect("/contatos")
      .withSession("name" -> name)
      .flashing("sucesso" -> "Login realizado com sucesso.")
    }
    def logout = Action {
    Redirect("/")
      .withNewSession
      .flashing("sucesso" -> "Logout realizado com sucesso.")
    }
    
    var id = 0L
    def nextId() : Long = {
        id += 1
        id
    }
    val contatoForm = Form(
        mapping(
          "id" -> ignored(0L),
          "nome" -> nonEmptyText,
          "email" -> email,
          "telefone" -> nonEmptyText
        )(Contato.apply)(Contato.unapply)
    )
    val novaLista = new HashMap[Long, Contato]
    def exibirContato(id: Int) : Contato = {
        novaLista(id)
    }
    def atualizarContato(id: Long, c: Contato) = {
        c.id = id
        novaLista(id) = c
    }
    def exibirLista() : List[Contato] = {
        novaLista.values.toList
    }
    def atualizarLista(c: Contato) = {
        val id = nextId()
        c.id = id
        novaLista += id -> c
    }
    def index = Action {
      Ok(views.html.index("Bem-vindo."))
    }
    def contatos = Action { implicit request =>
        request.session.get("name") match {
            case Some(name) => Ok(views.html.contatos(exibirLista(), name))
            case None => Unauthorized
        }

    }
    def create = Action {
        Ok(views.html.criar(contatoForm))
    }
    def save = Action { implicit request =>
        contatoForm.bindFromRequest.fold(
        formWithErrors => {
            BadRequest(views.html.criar(formWithErrors))
        },
        contato => {
            atualizarLista(contato)
            Redirect(routes.HomeController.contatos)
        })
    }
    def edit(id: Long) = Action { implicit request =>
        val formContatoPreenchido = contatoForm.fill(exibirContato(id.toInt))
        Ok(views.html.editar(formContatoPreenchido, exibirContato(id.toInt)))
    }
    def update(id: Long) = Action { implicit request =>
        contatoForm.bindFromRequest.fold(
        formWithErrors => {
            BadRequest(views.html.editar(formWithErrors, exibirContato(id.toInt)))
        },
        contato => {
            atualizarContato(id, contato)
            Redirect(routes.HomeController.contatos)
        })
    }
    def delete(id: Long) = Action { implicit request =>
        novaLista.remove(id)
        Redirect(routes.HomeController.contatos)
    }
}
