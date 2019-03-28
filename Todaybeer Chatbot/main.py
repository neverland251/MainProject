import time
import logging
import time

# import chatbot.ideal_worldcup
import numpy as np
from telegram import InlineKeyboardButton, InlineKeyboardMarkup
from telegram.ext import Updater, CommandHandler, MessageHandler, Filters, CallbackQueryHandler, ConversationHandler

import chatbot.todaybeer
from chatbot.ideal_worldcup import *

# Updater: to receive the updates from Telegram and to deliver them to said dispatcher.
# CommandHandler: Commands are Telegram messages that start with /, optionally followed by an @ and the bot’s name.
# MessageHandler:  text, media or status updates
# CallbackQueryHandler: handle Telegram callback queries

# from telegram import (ReplyKeyboardMarkup, ReplyKeyboardRemove)

token = "732252639:AAEP1ijq7f-2unpYd5LsETYoeQbDgX60Ev8"

# 터미널에 봇에서 일어날 정보 전달
logging.basicConfig(format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
                    level=logging.INFO)
logger = logging.getLogger(__name__)


# chat_id = bot.getUpdates()[-1].message #가장 최근에 온 메세지의 chat id를 가져옵니다

# 첫 시작
def start(bot, update):
    # username 받기
    print(update.message.chat.username)
    t = (
                "안녕 %s!, 나는 맥주 추천봇이야!" + "\n" + "맥주 추천하기 전해 너의 맥주 취향에 대해 알아볼까?" + "\n" + "/ideal_beer 를 눌러줘.") % update.message.chat.first_name
    bot.sendMessage(chat_id=update.message.chat_id, text=t)


# ideal = chatbot.ideal_worldcup.ideal_worldcup()
'''
def ideal_world(bot, update):
    userbeer = ideal.ideal_choice()
    print(userbeer)
    t = "너의 맥주" + str(userbeer[0]) + "야!"
    bot.send_message(chat_id=update.message.chat_id, text= t )
'''

graphs = chatbot.todaybeer.graphs
chat = chatbot.todaybeer.todaybeer()
worldcup = chatbot.ideal_worldcup.ideal_worldcup()

# 문장 입력
def beer_intention(bot, update):
    corpus = update.message.text
    print(corpus)
    if corpus.startswith("/"):
        pass
    else:
        with graphs.as_default():
            dict_numb = chatbot.todaybeer.vocabulary["하이네켄"]
            user = chat.model.layers[2].get_weights()[0][dict_numb][np.newaxis, :]
            return_classes = chat.engine(corpus, 0, user)
        print(return_classes)
    t = "너의 맥주 타입은" + str(return_classes[0]) + "야!"
    bot.send_message(chat_id=update.message.chat_id, text=t)


def who(bot, update):
    t = "나는 맥주 데이터를 모아 분석해서 오늘 너의 기분에 따라 맥주 추천해주는 똑똑한 너의 술친구야!!" + "\n" + "이제 날 알겠지?" + "\n" + "/test 를 누르면 내가 맥주 하나 추천해줄게"
    bot.send_message(chat_id=update.message.chat_id, text=t)


# 버튼 메뉴 설정
def build_menu(buttons, n_cols, header_buttons=None, footer_buttons=None):
    menu = [buttons[i:i + n_cols] for i in range(0, len(buttons), n_cols)]
    # 첫번째 버튼
    if header_buttons:
        menu.insert(0, header_buttons)
    # 두버내 버튼
    if footer_buttons:
        menu.append(footer_buttons)
    return menu


# /test commend
def test(bot, update):
    t = "오늘은 날씨가 추우니깐 이런 날은 진한~ 흑맥주가 잘 어울려!"
    bot.sendMessage(chat_id=update.message.chat_id, text=t)
    time.sleep(0.3)
    bot.send_photo(chat_id=update.message.chat_id,
                   photo='https://www.gwine.com/images/labels/guinness-guinness-extra-stout.gif')
    time.sleep(0.3)
    t1 = "기네스 어때?!"
    bot.send_message(chat_id=update.message.chat_id, text=t1)
    time.sleep(0.3)
    # 키보드에 대답 넣기
    show_list = []
    show_list.append(InlineKeyboardButton("좋아", callback_data="좋아"))
    show_list.append(InlineKeyboardButton("별로야", callback_data="별로야"))
    show_markup = InlineKeyboardMarkup(build_menu(show_list, len(show_list) - 1))  # make markup
    update.message.reply_text("내 추천이 어떤지 알려 줄래?", reply_markup=show_markup)
    return 'test1' # 'test2' 테스트


# 버튼의 callback 값에 따라 답변 제공
def callback_get(bot, update):
    print("callback")
    print("update", update)
    if update.callback_query.data == "좋아":
        bot.edit_message_text(text="진짜? 내 추천 좋지?! 오늘 술 잘 마시고 지나친 음주는 몸에 안 좋은 거 알지?!" + "\n" + "다음에 또 놀러와!",
                              chat_id=update.callback_query.message.chat_id,
                              message_id=update.callback_query.message.message_id)

    if update.callback_query.data == '별로야':
        bot.edit_message_text(text="솔직한 의견 고마워" + "\n" + "다음에 또 놀러와!",
                              chat_id=update.callback_query.message.chat_id,
                              message_id=update.callback_query.message.message_id)
a = []

def callback_get2(bot, update):

    print('Handler2 Work!')


def sentence(bot, update):
    corpus = update.message.text
    print(corpus)
    return_classes = chat.engine(corpus)
    bot.send_message(chat_id=update.message.chat_id, text=return_classes)


def ideal_answer(bot, update, iteration=5):
    tries = 0
    # merged_list = world_cup.merged_lists
    select = random.choice(merged.columns)
    selected_vector = merged[select]
    versus, versus_vector = world_cup.cosine_sim_least(merged_lists, selected_vector)
    # send_message
    # input_value
    if answer == "1":
        return_value = selected_vector
        unselected_name = versus
    if answer == "2":
        return_value = versus_vector
        unselected_name = select
    merged_list = np.delete(merged_list, np.where(merged_list == unselected_name)[0][0])

    while tries <= iteration - 1:
        select, selected_vector = world_cup.cosine_sim_most(merged_list, return_value)
        versus, versus_vector = world_cup.cosine_sim_least(merged_list, return_value)
        # send_message
        # input_value
        if answer == "1":
            return_value = ((return_value - selected_vector) / 2 + selected_vector)
            unselected_name = versus
        if answer == "2":
            return_value = ((return_value - versus_vector) / 2 + versus_vector)
            unselected_name = select
        merged_list = np.delete(merged_list, np.where(merged_list == unselected_name)[0][0])
    user = return_value
    print('test', retv, end='....')


# command & function 활성
def main():
    updater = Updater(token=token)
    dp = updater.dispatcher
    conv_handler = ConversationHandler(
        entry_points=[CommandHandler('start', start),
                      MessageHandler(Filters.text, beer_intention),
                      CommandHandler('who', who),
                      CommandHandler('test', test),
                      CommandHandler("sentence", sentence),
                      CommandHandler("idel_test", ideal_answer)
                      ],
        states={
            'test1': [CallbackQueryHandler(callback_get)],
            'test2': [CallbackQueryHandler(callback_get2)]
        },
        fallbacks=[CommandHandler('start', start)]
    )
    # add your handlers here
    # dp.add_handler(CommandHandler("start", start))
    # dp.add_handler(MessageHandler(Filters.text, beer_intention))
    # dp.add_handler(CommandHandler('who', who))
    # dp.add_handler(CommandHandler('test', test))
    # dp.add_handler(CommandHandler("sentence", sentence))
    # dp.add_handler(CommandHandler("idel_test", ideal_answer))
    dp.add_handler(conv_handler)

    # log all errors
    # dp.add_error_handler(error)
    # polling시작, 걸리는 시간 최대치 정해줌 너무 낮은 경우는 poll이 제대로 작동이 안됨
    # clean =true 기존의 텔레그램 서버에 저장되어있던 업데이트 사항 지우기
    updater.start_polling(timeout=3)
    # idle은 updater가 종료되지 않고 계속 실행
    updater.idle()


if __name__ == '__main__':
    main()
