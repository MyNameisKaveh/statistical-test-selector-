// DOM Elements
const questionTextElement = document.getElementById('question-text');
const optionsContainerElement = document.getElementById('options-container');
const resultContainerElement = document.getElementById('result-container');
const testNameElement = document.getElementById('test-name');
const testDescriptionElement = document.getElementById('test-description');
const testWhenToUseElement = document.getElementById('test-when-to-use');
const rCodeElement = document.getElementById('r-code');
const rCodeNotesElement = document.getElementById('r-code-notes');
const resetButtonElement = document.getElementById('reset-button');
const questionContainerElement = document.getElementById('question-container');

// --- Decision Tree Data Structure ---
const decisionTree = {
    start: {
        question: "نوع متغیر وابسته اصلی (Dependent Variable) شما چیست؟",
        options: [
            { text: "پیوسته (Interval / Ratio)", nextNode: "q_dv_continuous_num_dvs" },
            { text: "طبقه‌ای / دسته‌ای (Categorical: Nominal / Ordinal)", nextNode: "q_dv_categorical_num_dvs" },
            { text: "شمارشی (Count)", nextNode: "q_dv_count_num_dvs" }
        ]
    },
    q_dv_continuous_num_dvs: {
        question: "چند متغیر وابسته پیوسته دارید؟",
        options: [
            { text: "یک متغیر وابسته پیوسته", nextNode: "q_one_dv_continuous_num_ivs" },
            { text: "دو یا چند متغیر وابسته پیوسته", nextNode: "q_multi_dv_continuous_num_ivs" }
        ]
    },
    q_one_dv_continuous_num_ivs: {
        question: "چند متغیر مستقل (Independent Variable) اصلی دارید؟",
        options: [
            { text: "هیچ (مقایسه میانگین با یک مقدار ثابت یا بررسی توزیع)", nextNode: "q_one_dv_continuous_no_iv_goal" },
            { text: "یک متغیر مستقل", nextNode: "q_one_dv_one_iv_type" },
            { text: "دو یا چند متغیر مستقل", nextNode: "q_one_dv_multi_iv_type" }
        ]
    },
    q_one_dv_continuous_no_iv_goal: {
        question: "هدف شما چیست؟",
        options: [
            { text: "مقایسه میانگین نمونه با یک مقدار مشخص/فرضی", nextNode: "test_one_sample_t_test" },
            { text: "بررسی نرمال بودن توزیع متغیر", nextNode: "test_shapiro_wilk" }
        ]
    },
    q_one_dv_one_iv_type: {
        question: "نوع متغیر مستقل شما چیست؟",
        options: [
            { text: "طبقه‌ای با دو سطح/گروه (مثلاً زن/مرد، درمان/کنترل)", nextNode: "q_one_dv_one_iv_cat2_paired" },
            { text: "طبقه‌ای با سه یا چند سطح/گروه (مثلاً سه روش درمانی مختلف)", nextNode: "q_one_dv_one_iv_cat_multi_paired" },
            { text: "پیوسته (Interval / Ratio)", nextNode: "test_simple_linear_regression" }
        ]
    },
    q_one_dv_one_iv_cat2_paired: {
        question: "آیا مشاهدات/گروه‌ها مستقل هستند یا جفت شده/وابسته (repeated measures)؟",
        options: [
            { text: "مستقل (Independent samples, e.g., different subjects in each group)", nextNode: "test_independent_samples_t_test" },
            { text: "جفت شده / وابسته (Paired samples / Repeated measures, e.g., same subjects measured twice)", nextNode: "test_paired_samples_t_test" }
        ]
    },
    q_one_dv_one_iv_cat_multi_paired: {
        question: "آیا مشاهدات/گروه‌ها مستقل هستند یا جفت شده/وابسته (repeated measures)؟",
        options: [
            { text: "مستقل", nextNode: "test_one_way_anova" },
            { text: "جفت شده / وابسته", nextNode: "test_one_way_repeated_measures_anova" }
        ]
    },
    q_one_dv_multi_iv_type: {
        question: "ماهیت متغیرهای مستقل شما چگونه است؟",
        options: [
            { text: "همگی طبقه‌ای (Categorical)", nextNode: "q_one_dv_multi_iv_all_cat_interaction" },
            { text: "همگی پیوسته (Continuous)", nextNode: "test_multiple_regression" },
            { text: "ترکیبی از پیوسته و طبقه‌ای", nextNode: "test_ancova_or_regression_with_dummies" }
        ]
    },
    q_one_dv_multi_iv_all_cat_interaction: {
        question: "آیا علاقه‌مند به بررسی اثرات متقابل (interaction effects) بین متغیرهای مستقل طبقه‌ای هستید؟",
        options: [
            { text: "بله، می‌خواهم اثرات متقابل را بررسی کنم", nextNode: "q_one_dv_multi_iv_all_cat_paired_interaction" },
            { text: "خیر، فقط اثرات اصلی (main effects) مد نظر است", nextNode: "q_one_dv_multi_iv_all_cat_paired_no_interaction" }
        ]
    },
    q_one_dv_multi_iv_all_cat_paired_interaction: {
        question: "آیا داده‌ها شامل اندازه‌گیری‌های مکرر (repeated measures) روی حداقل یکی از فاکتورها هستند؟",
        options: [
            { text: "خیر، تمام فاکتورها بین آزمودنی (between-subjects) هستند.", nextNode: "test_factorial_anova_independent" },
            { text: "بله، حداقل یک فاکتور درون آزمودنی (within-subjects / repeated measures) است.", nextNode: "test_factorial_anova_repeated_measures" },
            { text: "طرح مختلط (Mixed design): هم فاکتور بین آزمودنی و هم درون آزمودنی دارم.", nextNode: "test_mixed_anova" }
        ]
    },
    q_one_dv_multi_iv_all_cat_paired_no_interaction: {
        question: "آیا داده‌ها شامل اندازه‌گیری‌های مکرر (repeated measures) روی حداقل یکی از فاکتورها هستند؟",
        options: [
            { text: "خیر، تمام فاکتورها بین آزمودنی (between-subjects) هستند.", nextNode: "test_anova_main_effects_only_independent" },
            { text: "بله، حداقل یک فاکتور درون آزمودنی (within-subjects / repeated measures) است.", nextNode: "test_repeated_measures_anova_main_effects_only" }
        ]
    },
    q_multi_dv_continuous_num_ivs: {
        question: "چند متغیر مستقل (Independent Variable) اصلی دارید؟",
        options: [
            { text: "یک متغیر مستقل", nextNode: "q_multi_dv_one_iv_type" },
            { text: "دو یا چند متغیر مستقل", nextNode: "q_multi_dv_multi_iv_type" }
        ]
    },
    q_multi_dv_one_iv_type: {
        question: "نوع متغیر مستقل شما چیست؟",
        options: [
            { text: "طبقه‌ای (Categorical)", nextNode: "test_manova" },
            { text: "پیوسته (Continuous)", nextNode: "test_multivariate_multiple_regression" }
        ]
    },
    q_multi_dv_multi_iv_type: {
        question: "ماهیت متغیرهای مستقل شما چگونه است؟",
        options: [
            { text: "همگی طبقه‌ای (Categorical)", nextNode: "test_factorial_manova" },
            { text: "همگی پیوسته (Continuous) یا ترکیبی", nextNode: "test_multivariate_multiple_regression" }
        ]
    },
    q_dv_categorical_num_dvs: {
        question: "چند متغیر وابسته طبقه‌ای دارید؟",
        options: [
            { text: "یک متغیر وابسته طبقه‌ای", nextNode: "q_one_dv_categorical_num_ivs" },
            { text: "دو یا چند متغیر وابسته طبقه‌ای", nextNode: "test_log_linear_model_or_mca" }
        ]
    },
    q_one_dv_categorical_num_ivs: {
        question: "چند متغیر مستقل (Independent Variable) اصلی دارید؟",
        options: [
            { text: "هیچ (بررسی توزیع فراوانی یا نیکویی برازش با یک توزیع نظری)", nextNode: "q_one_dv_cat_no_iv_goal" },
            { text: "یک متغیر مستقل", nextNode: "q_one_dv_one_iv_cat_type_of_iv" },
            { text: "دو یا چند متغیر مستقل", nextNode: "q_one_dv_multi_iv_cat_type_of_ivs" }
        ]
    },
    q_one_dv_cat_no_iv_goal: {
        question: "هدف شما چیست؟",
        options: [
            { text: "مقایسه فراوانی‌های مشاهده شده با فراوانی‌های مورد انتظار (نیکویی برازش)", nextNode: "test_chi_square_goodness_of_fit" },
            { text: "توصیف فراوانی‌ها و درصدها", nextNode: "info_descriptive_frequencies" }
        ]
    },
    q_one_dv_one_iv_cat_type_of_iv: {
        question: "نوع متغیر مستقل شما چیست؟",
        options: [
            { text: "طبقه‌ای (Categorical)", nextNode: "q_one_dv_cat_one_iv_cat_paired" },
            { text: "پیوسته (Continuous)", nextNode: "test_logistic_regression_simple" },
            { text: "ترتیبی (Ordinal)", nextNode: "test_ordinal_logistic_regression" }
        ]
    },
    q_one_dv_cat_one_iv_cat_paired: {
        question: "آیا مشاهدات/گروه‌ها مستقل هستند یا جفت شده/وابسته؟",
        options: [
            { text: "مستقل (Independent samples)", nextNode: "test_chi_square_test_of_independence" },
            { text: "جفت شده / وابسته (Paired samples / Repeated measures)", nextNode: "test_mcnemar_test" }
        ]
    },
    q_one_dv_multi_iv_cat_type_of_ivs: {
        question: "ماهیت متغیرهای مستقل شما چگونه است؟ (نوع متغیر وابسته طبقه‌ای شما چیست؟ دو سطحی یا چند سطحی؟)",
        options: [
            { text: "متغیرهای مستقل همگی طبقه‌ای (DV دو سطحی)", nextNode: "test_logistic_regression_multi_cat_iv" },
            { text: "متغیرهای مستقل همگی پیوسته یا ترکیبی (DV دو سطحی)", nextNode: "test_multiple_logistic_regression" },
            { text: "متغیر وابسته چند سطحی اسمی (Nominal)", nextNode: "test_multinomial_logistic_regression" },
            { text: "متغیر وابسته چند سطحی ترتیبی (Ordinal)", nextNode: "test_ordinal_logistic_regression_multi_iv" }
        ]
    },
    q_dv_count_num_dvs: {
        question: "چند متغیر مستقل (Independent Variable) اصلی دارید؟",
        options: [
            { text: "هیچ (مقایسه میانگین شمارش با یک مقدار ثابت یا بررسی توزیع)", nextNode: "q_one_dv_count_no_iv_goal" },
            { text: "یک یا چند متغیر مستقل (برای مدل‌سازی نرخ شمارش)", nextNode: "q_one_dv_count_iv_exposure" }
        ]
    },
    q_one_dv_count_no_iv_goal: {
        question: "هدف شما چیست؟",
        options: [
            { text: "مقایسه نرخ شمارش مشاهده شده با یک نرخ مورد انتظار", nextNode: "test_poisson_goodness_of_fit" },
            { text: "توصیف توزیع شمارش", nextNode: "info_descriptive_count" }
        ]
    },
    q_one_dv_count_iv_exposure: {
        question: "آیا متغیر جبرانی (exposure variable) یا دوره زمانی مشاهده برای هر واحد یکسان است یا متفاوت؟",
        options: [
            { text: "یکسان است یا می‌خواهم نرخ را مستقیماً مدل کنم.", nextNode: "q_one_dv_count_iv_dispersion" },
            { text: "متفاوت است و می‌خواهم آن را در مدل لحاظ کنم (offset).", nextNode: "q_one_dv_count_iv_dispersion_offset" }
        ]
    },
    q_one_dv_count_iv_dispersion: {
        question: "آیا پراکندگی بیش از حد (Overdispersion: واریانس بزرگتر از میانگین) در داده‌های شمارشی شما محتمل است؟",
        options: [
            { text: "خیر، یا مطمئن نیستم (شروع با پواسون).", nextNode: "test_poisson_regression" },
            { text: "بله، پراکندگی بیش از حد وجود دارد یا محتمل است.", nextNode: "test_negative_binomial_regression" },
            { text: "داده‌ها شامل تعداد زیادی صفر هستند (Zero-inflated).", nextNode: "test_zero_inflated_poisson_or_nb" }
        ]
    },
    q_one_dv_count_iv_dispersion_offset: {
        question: "آیا پراکندگی بیش از حد (Overdispersion) در داده‌های شمارشی شما محتمل است؟ (با لحاظ کردن متغیر جبرانی)",
        options: [
            { text: "خیر، یا مطمئن نیستم (شروع با پواسون با offset).", nextNode: "test_poisson_regression_offset" },
            { text: "بله، پراکندگی بیش از حد وجود دارد یا محتمل است (با offset).", nextNode: "test_negative_binomial_regression_offset" }
        ]
    },
    test_one_sample_t_test: {
        isFinal: true,
        name: "آزمون تی تک نمونه‌ای (One-Sample t-test)",
        description: "برای مقایسه میانگین یک گروه با یک مقدار مشخص (مقدار فرضی یا میانگین جامعه).",
        whenToUse: "زمانی که یک متغیر وابسته پیوسته دارید و می‌خواهید بدانید آیا میانگین آن در نمونه شما به طور معناداری با یک مقدار ثابت (μ0) تفاوت دارد یا خیر. داده‌ها باید تقریباً نرمال توزیع شده باشند.",
        r_code: `
t.test(data$variable, mu = mu0)
        `,
        r_code_notes: "<p><code>data$variable</code> متغیر مورد بررسی شماست.</p><p><code>mu = mu0</code> مقدار فرضی برای مقایسه است (مثلاً <code>mu = 50</code>).</p><p>پیش‌فرض اصلی نرمال بودن داده‌ها یا حجم نمونه بزرگ است.</p>"
    },
    test_shapiro_wilk: {
        isFinal: true,
        name: "آزمون شاپیرو-ویلک (Shapiro-Wilk test for normality)",
        description: "برای بررسی اینکه آیا یک نمونه از یک توزیع نرمال آمده است یا خیر.",
        whenToUse: "زمانی که می‌خواهید پیش‌فرض نرمال بودن داده‌ها را برای آزمون‌های پارامتریک (مانند t-test یا ANOVA) بررسی کنید. این آزمون به حجم نمونه حساس است.",
        r_code: `
shapiro.test(data$variable)
        `,
        r_code_notes: "<p>اگر p-value بزرگتر از سطح معناداری (مثلاً 0.05) باشد، دلیلی برای رد فرض نرمال بودن وجود ندارد.</p><p>برای ارزیابی بصری نرمال بودن، از نمودار Q-Q Plot (<code>qqnorm()</code>, <code>qqline()</code>) و هیستوگرام نیز استفاده کنید.</p>"
    },
    test_independent_samples_t_test: {
        isFinal: true,
        name: "آزمون تی دو نمونه مستقل (Independent Samples t-test)",
        description: "برای مقایسه میانگین‌های دو گروه مستقل.",
        whenToUse: "زمانی که یک متغیر وابسته پیوسته و یک متغیر مستقل طبقه‌ای با دو سطح (دو گروه مستقل) دارید. پیش‌فرض‌ها شامل نرمال بودن داده‌ها در هر گروه و برابری واریانس‌ها (هرچند آزمون ولچ t-test به این پیش‌فرض حساس نیست).",
        r_code: `
t.test(dependent_var ~ group_var, data = data, var.equal = FALSE)
        `,
        r_code_notes: "<p><code>var.equal = FALSE</code> (پیش‌فرض در R) آزمون ولچ را اجرا می‌کند که در صورت نابرابری واریانس‌ها معتبرتر است.</p><p>برای بررسی برابری واریانس‌ها می‌توانید از آزمون Levene (<code>car::leveneTest()</code>) استفاده کنید.</p>"
    },
    test_paired_samples_t_test: {
        isFinal: true,
        name: "آزمون تی نمونه‌های جفت شده (Paired Samples t-test)",
        description: "برای مقایسه میانگین‌های دو اندازه‌گیری وابسته (جفت شده) روی یک گروه از آزمودنی‌ها.",
        whenToUse: "زمانی که یک متغیر وابسته پیوسته دارید که دو بار روی یک گروه از افراد یا واحدهای جفت شده اندازه‌گیری شده است (مثلاً قبل و بعد از یک مداخله). پیش‌فرض این است که تفاوت بین جفت‌ها نرمال توزیع شده باشد.",
        r_code: `
t.test(data$measurement1, data$measurement2, paired = TRUE)
        `,
        r_code_notes: "<p><code>paired = TRUE</code> مشخص می‌کند که آزمون برای نمونه‌های جفت شده است.</p><p>اطمینان حاصل کنید که ترتیب دو متغیر در تابع <code>t.test</code> درست است، به خصوص اگر جهت تفاوت برایتان مهم است.</p>"
    },
    test_simple_linear_regression: {
        isFinal: true,
        name: "رگرسیون خطی ساده (Simple Linear Regression)",
        description: "برای بررسی رابطه خطی بین یک متغیر وابسته پیوسته و یک متغیر مستقل پیوسته.",
        whenToUse: "زمانی که می‌خواهید پیش‌بینی کنید یا توضیح دهید که چگونه یک متغیر مستقل پیوسته بر یک متغیر وابسته پیوسته تاثیر می‌گذارد.",
        r_code: `
model <- lm(dependent_var ~ independent_var, data = data)
summary(model)
        `,
        r_code_notes: "<p><code>lm</code> مخفف linear model است.</p><p><code>summary(model)</code> ضرایب، خطای استاندارد، آماره t، p-value و R-squared را نشان می‌دهد.</p><p>پیش‌فرض‌ها شامل خطی بودن رابطه، استقلال خطاها، همسانی واریانس خطاها (homoscedasticity) و نرمال بودن توزیع خطاها است.</p>"
    },
    test_one_way_anova: {
        isFinal: true,
        name: "تحلیل واریانس یک‌طرفه (One-Way ANOVA)",
        description: "برای مقایسه میانگین‌های سه یا چند گروه مستقل.",
        whenToUse: "زمانی که یک متغیر وابسته پیوسته و یک متغیر مستقل طبقه‌ای با سه یا چند سطح (گروه مستقل) دارید. پیش‌فرض‌ها شامل نرمال بودن داده‌ها در هر گروه، برابری واریانس‌ها بین گروه‌ها (homoscedasticity) و استقلال مشاهدات است.",
        r_code: `
model <- aov(dependent_var ~ group_var, data = data)
summary(model)
        `,
        r_code_notes: "<p>اگر متغیر گروه شما عددی است، ابتدا آن را به <code>factor</code> تبدیل کنید.</p><p>اگر نتیجه ANOVA معنادار بود (p-value کوچک)، از آزمون‌های تعقیبی مانند Tukey HSD برای مقایسه جفتی میانگین‌ها استفاده کنید.</p><p>برای بررسی برابری واریانس‌ها از آزمون Levene (<code>car::leveneTest()</code>) استفاده کنید.</p>"
    },
    test_one_way_repeated_measures_anova: {
        isFinal: true,
        name: "تحلیل واریانس یک‌طرفه با اندازه‌گیری‌های مکرر (One-Way Repeated Measures ANOVA)",
        description: "برای مقایسه میانگین‌ها در سه یا چند سطح از یک فاکتور درون‌آزمودنی (within-subjects factor).",
        whenToUse: "زمانی که یک متغیر وابسته پیوسته دارید که سه بار یا بیشتر روی یک گروه از افراد یا واحدهای مشابه اندازه‌گیری شده است (مثلاً در سه نقطه زمانی مختلف). پیش‌فرض کرویت (sphericity) مهم است.",
        r_code: `
library(rstatix)
model <- anova_test(data = data_long, dv = value, wid = id, within = time)
print(model)
        `,
        r_code_notes: "<p>داده‌ها باید در فرمت 'long' باشند: هر ردیف یک مشاهده، با ستون‌هایی برای شناسه فرد، فاکتور درون‌آزمودنی و مقدار متغیر وابسته.</p><p>پیش‌فرض کرویت (sphericity) با آزمون ماچلی (Mauchly's Test) بررسی می‌شود. اگر نقض شود، از مقادیر p تصحیح شده (مانند Greenhouse-Geisser) استفاده کنید.</p>"
    },
    test_multiple_regression: {
        isFinal: true,
        name: "رگرسیون چندگانه (Multiple Regression)",
        description: "برای بررسی رابطه بین یک متغیر وابسته پیوسته و دو یا چند متغیر مستقل (پیوسته یا طبقه‌ای).",
        whenToUse: "زمانی که می‌خواهید پیش‌بینی کنید یا توضیح دهید که چگونه چندین متغیر مستقل بر یک متغیر وابسته پیوسته تاثیر می‌گذارند. متغیرهای مستقل طبقه‌ای باید به صورت متغیرهای ساختگی (dummy variables) وارد شوند.",
        r_code: `
model <- lm(dependent_var ~ independent_var1 + independent_var2 + independent_var_categorical, data = data)
summary(model)
        `,
        r_code_notes: "<p>پیش‌فرض‌ها مشابه رگرسیون ساده است اما شامل عدم هم‌خطی چندگانه شدید بین متغیرهای مستقل نیز می‌شود (با VIF بررسی کنید).</p><p>اگر متغیرهای مستقل طبقه‌ای دارید، مطمئن شوید که در R به عنوان <code>factor</code> تعریف شده‌اند.</p>"
    },
    test_ancova_or_regression_with_dummies: {
        isFinal: true,
        name: "رگرسیون با متغیرهای مستقل پیوسته و طبقه‌ای (ANCOVA به عنوان حالت خاص)",
        description: "برای بررسی تاثیر متغیرهای مستقل طبقه‌ای و پیوسته بر یک متغیر وابسته پیوسته. ANCOVA به طور خاص به مقایسه میانگین‌های تعدیل شده گروه‌ها پس از کنترل اثر یک یا چند متغیر پیوسته (covariate) می‌پردازد.",
        whenToUse: "زمانی که می‌خواهید اثر یک یا چند متغیر مستقل طبقه‌ای را بر متغیر وابسته پیوسته بررسی کنید، در حالی که اثر یک یا چند متغیر مستقل پیوسته (covariates) را کنترل می‌کنید. یا به طور کلی، مدلی با ترکیبی از انواع متغیرهای مستقل دارید.",
        r_code: `
model_ancova <- lm(dependent_var ~ covariate_continuous + group_factor, data = data)
summary(model_ancova)
        `,
        r_code_notes: "<p>در ANCOVA، یک پیش‌فرض مهم، همگنی شیب‌های رگرسیون (homogeneity of regression slopes) است، یعنی عدم وجود اثر متقابل معنادار بین همبسته و فاکتور گروه‌بندی. این را با وارد کردن جمله <code>covariate_continuous * group_factor</code> در مدل بررسی کنید.</p><p>اگر همبستگی زیادی بین همبسته(ها) و متغیرهای مستقل طبقه‌ای وجود داشته باشد، تفسیر نتایج دشوار می‌شود.</p>"
    },
    test_factorial_anova_independent: {
        isFinal: true,
        name: "تحلیل واریانس عاملی (بین آزمودنی‌ها) (Factorial ANOVA - Independent Groups)",
        description: "برای بررسی اثرات اصلی و متقابل دو یا چند متغیر مستقل طبقه‌ای (فاکتور) بر یک متغیر وابسته پیوسته، زمانی که گروه‌ها مستقل هستند.",
        whenToUse: "زمانی که بیش از یک متغیر مستقل طبقه‌ای دارید و می‌خواهید تاثیر هر یک به تنهایی (اثر اصلی) و تاثیر ترکیب آن‌ها (اثر متقابل) را بر متغیر وابسته پیوسته بررسی کنید. تمام فاکتورها بین آزمودنی هستند (هر آزمودنی فقط در یک ترکیب از سطوح فاکتورها قرار دارد).",
        r_code: `
model <- aov(dependent_var ~ factorA * factorB, data = data)
summary(model)
        `,
        r_code_notes: "<p><code>factorA * factorB</code> شامل اثرات اصلی factorA، factorB و اثر متقابل factorA:factorB می‌شود.</p><p>پیش‌فرض‌ها مشابه ANOVA یک‌طرفه (نرمال بودن، همسانی واریانس‌ها، استقلال مشاهدات).</p><p>تفسیر اثرات اصلی در حضور اثر متقابل معنادار باید با احتیاط انجام شود.</p>"
    },
    test_factorial_anova_repeated_measures: {
        isFinal: true,
        name: "تحلیل واریانس عاملی با اندازه‌گیری‌های مکرر (Factorial Repeated Measures ANOVA)",
        description: "برای بررسی اثرات اصلی و متقابل دو یا چند فاکتور درون‌آزمودنی (within-subjects factors) بر یک متغیر وابسته پیوسته.",
        whenToUse: "زمانی که بیش از یک فاکتور دارید که همگی به صورت مکرر روی یک گروه از آزمودنی‌ها اندازه‌گیری شده‌اند (مثلاً عملکرد در شرایط مختلف A و در زمان‌های مختلف B).",
        r_code: `
library(rstatix)
model <- anova_test(data = data_long, dv = dependent_var, wid = id, within = c(within_factor1, within_factor2))
print(model)
        `,
        r_code_notes: "<p>داده‌ها باید در فرمت 'long' باشند. هر ردیف یک مشاهده در یک ترکیب از سطوح فاکتورهای درون‌آزمودنی برای یک فرد.</p><p>پیش‌فرض کرویت (sphericity) برای هر اثر درون‌آزمودنی و اثرات متقابل آن‌ها بررسی می‌شود.</p>"
    },
    test_mixed_anova: {
        isFinal: true,
        name: "تحلیل واریانس طرح مختلط (Mixed ANOVA / Split-Plot ANOVA)",
        description: "برای بررسی اثرات فاکتور(های) بین‌آزمودنی و فاکتور(های) درون‌آزمودنی، و اثرات متقابل آن‌ها بر یک متغیر وابسته پیوسته.",
        whenToUse: "زمانی که حداقل یک متغیر مستقل طبقه‌ای بین‌آزمودنی (مثلاً گروه درمان و کنترل) و حداقل یک متغیر مستقل طبقه‌ای درون‌آزمودنی (مثلاً اندازه‌گیری در زمان‌های مختلف) دارید.",
        r_code: `
library(rstatix)
model <- anova_test(data = data_long, dv = dependent_var, wid = id, between = between_factor, within = within_factor)
print(model)
        `,
        r_code_notes: "<p>داده‌ها باید در فرمت 'long' باشند.</p><p>این آزمون اثرات اصلی فاکتور بین‌آزمودنی، فاکتور درون‌آزمودنی، و اثر متقابل آن‌ها را بررسی می‌کند.</p><p>پیش‌فرض کرویت برای اثرات درون‌آزمودنی و همسانی واریانس‌ها برای اثرات بین‌آزمودنی مهم است.</p>"
    },
    test_anova_main_effects_only_independent: {
        isFinal: true,
        name: "تحلیل واریانس عاملی (فقط اثرات اصلی، گروه‌های مستقل)",
        description: "برای بررسی اثرات اصلی دو یا چند متغیر مستقل طبقه‌ای (فاکتور) بر یک متغیر وابسته پیوسته، زمانی که گروه‌ها مستقل هستند و فرض می‌شود اثر متقابلی وجود ندارد یا مورد علاقه نیست.",
        whenToUse: "زمانی که بیش از یک متغیر مستقل طبقه‌ای دارید و فقط می‌خواهید تاثیر هر یک به تنهایی (اثر اصلی) را بر متغیر وابسته پیوسته بررسی کنید و علاقه‌ای به اثر متقابل آن‌ها ندارید یا دلایل نظری برای عدم وجود آن دارید.",
        r_code: `
model_main_effects <- aov(dependent_var ~ factorA + factorB, data = data)
summary(model_main_effects)
        `,
        r_code_notes: "<p><code>factorA + factorB</code> فقط اثرات اصلی factorA و factorB را در مدل لحاظ می‌کند.</p><p>این مدل ساده‌تر از مدلی است که شامل اثر متقابل است و تنها در صورتی باید استفاده شود که دلایل قوی برای نادیده گرفتن اثر متقابل وجود داشته باشد.</p>"
    },
    test_repeated_measures_anova_main_effects_only: {
        isFinal: true,
        name: "ANOVA اندازه‌گیری‌های مکرر (فقط اثرات اصلی)",
        description: "بررسی اثرات اصلی چند فاکتور درون‌آزمودنی بر متغیر وابسته پیوسته، بدون بررسی اثرات متقابل.",
        whenToUse: "زمانی که چند فاکتور درون‌آزمودنی دارید و تنها به اثرات اصلی آن‌ها علاقه‌مندید و نه به اثرات متقابل آن‌ها.",
        r_code: `
library(rstatix)
model <- anova_test(data = data_long, dv = dependent_var, wid = id, within = list(within_factor1, within_factor2))
print(model)
        `,
        r_code_notes: "<p>اجرای یک مدل RM ANOVA که فقط اثرات اصلی را بدون اثرات متقابل ارزیابی کند، می‌تواند کمی پیچیده‌تر باشد و اغلب نیاز به استفاده از مدل‌های خطی مختلط (mixed models) دارد.</p><p>روش ارائه شده در کد، ساده‌ترین راه با <code>rstatix</code> است که تمام اثرات (از جمله متقابل) را گزارش می‌دهد و شما باید روی اثرات اصلی تمرکز کنید.</p>"
    },
    test_manova: {
        isFinal: true,
        name: "تحلیل واریانس چندمتغیره (MANOVA)",
        description: "برای مقایسه میانگین‌های چندین متغیر وابسته پیوسته بین دو یا چند گروه مستقل (تعریف شده توسط یک متغیر مستقل طبقه‌ای).",
        whenToUse: "زمانی که بیش از یک متغیر وابسته پیوسته دارید و می‌خواهید تاثیر یک متغیر مستقل طبقه‌ای را بر ترکیب خطی این متغیرهای وابسته بررسی کنید.",
        r_code: `
dependent_vars_matrix <- cbind(data$Y1, data$Y2)
model_manova <- manova(dependent_vars_matrix ~ Group, data = data)
summary(model_manova, test = "Pillai")
        `,
        r_code_notes: "<p><code>cbind()</code> برای ایجاد ماتریس متغیرهای وابسته استفاده می‌شود.</p><p>آماره Pillai's Trace اغلب به عنوان یک آماره قوی (robust) توصیه می‌شود.</p>"
    },
    test_factorial_manova: {
        isFinal: true,
        name: "تحلیل واریانس چندمتغیره عاملی (Factorial MANOVA)",
        description: "برای بررسی اثرات اصلی و متقابل دو یا چند متغیر مستقل طبقه‌ای بر چندین متغیر وابسته پیوسته.",
        whenToUse: "زمانی که بیش از یک متغیر وابسته پیوسته و بیش از یک متغیر مستقل طبقه‌ای دارید و می‌خواهید اثرات اصلی و متقابل فاکتورها را بر ترکیب خطی متغیرهای وابسته بررسی کنید.",
        r_code: `
dependent_vars_matrix <- cbind(data$Y1, data$Y2)
model_factorial_manova <- manova(dependent_vars_matrix ~ FactorA * FactorB, data = data)
summary(model_factorial_manova, test = "Pillai")
        `,
        r_code_notes: "<p><code>FactorA * FactorB</code> شامل اثرات اصلی و اثر متقابل می‌شود.</p><p>پیش‌فرض‌ها مشابه MANOVA یک‌طرفه است.</p>"
    },
    test_multivariate_multiple_regression: {
        isFinal: true,
        name: "رگرسیون چندگانه چندمتغیره (Multivariate Multiple Regression)",
        description: "برای بررسی رابطه بین چندین متغیر وابسته پیوسته و چندین متغیر مستقل (پیوسته یا طبقه‌ای).",
        whenToUse: "زمانی که می‌خواهید تاثیر چندین متغیر مستقل را بر بیش از یک متغیر وابسته پیوسته به طور همزمان مدل‌سازی کنید.",
        r_code: `
dependent_vars_matrix <- cbind(data$Y1, data$Y2)
model_mv_regr <- lm(dependent_vars_matrix ~ X1 + X2, data = data)
summary(model_mv_regr)
        `,
        r_code_notes: "<p>تابع <code>lm()</code> در R می‌تواند یک ماتریس از متغیرهای وابسته را بپذیرد.</p><p>خروجی <code>summary()</code> نتایج رگرسیون را برای هر متغیر وابسته به طور جداگانه نشان می‌دهد.</p>"
    },
    test_chi_square_goodness_of_fit: {
        isFinal: true,
        name: "آزمون خی‌دو نیکویی برازش (Chi-squared Goodness-of-Fit Test)",
        description: "برای مقایسه فراوانی‌های مشاهده شده یک متغیر طبقه‌ای با فراوانی‌های مورد انتظار (نظری).",
        whenToUse: "زمانی که یک متغیر طبقه‌ای دارید و می‌خواهید بدانید آیا توزیع فراوانی آن در نمونه شما با یک توزیع مشخص (مثلاً توزیع یکنواخت یا توزیع مشخص شده از قبل) مطابقت دارد یا خیر.",
        r_code: `
observed_frequencies <- table(data$categorical_var)
chisq.test(x = observed_frequencies, p = rep(1/length(observed_frequencies), length(observed_frequencies)))
        `,
        r_code_notes: "<p><code>observed_frequencies</code> یک جدول از فراوانی‌های سطوح متغیر طبقه‌ای شماست.</p><p><code>p</code> یک بردار از احتمالات مورد انتظار برای هر سطح است (باید جمعشان 1 شود).</p>"
    },
    info_descriptive_frequencies: {
        isFinal: true,
        name: "آمار توصیفی: جداول فراوانی و درصد",
        description: "برای توصیف توزیع یک متغیر طبقه‌ای.",
        whenToUse: "زمانی که می‌خواهید فراوانی (تعداد) و درصد هر سطح از یک متغیر طبقه‌ای را نمایش دهید.",
        r_code: `
frequency_table <- table(data$categorical_var)
percentage_table <- prop.table(frequency_table) * 100
print(frequency_table)
print(percentage_table)
        `,
        r_code_notes: "<p><code>table()</code> فراوانی‌ها را محاسبه می‌کند.</p><p><code>prop.table()</code> نسبت‌ها را محاسبه می‌کند که با ضرب در 100 به درصد تبدیل می‌شوند.</p>"
    },
    test_chi_square_test_of_independence: {
        isFinal: true,
        name: "آزمون خی‌دو استقلال (Chi-squared Test of Independence)",
        description: "برای بررسی اینکه آیا ارتباطی بین دو متغیر طبقه‌ای وجود دارد یا خیر.",
        whenToUse: "زمانی که دو متغیر طبقه‌ای دارید و می‌خواهید بدانید آیا این دو متغیر از یکدیگر مستقل هستند یا با هم ارتباط (association) دارند. برای جداول contingency 2x2 یا بزرگتر.",
        r_code: `
contingency_table <- table(data$categorical_var1, data$categorical_var2)
chisq.test(contingency_table)
        `,
        r_code_notes: "<p>ابتدا یک جدول توافقی (crosstabulation) از دو متغیر ایجاد می‌شود.</p><p>پیش‌فرض: تمام فراوانی‌های مورد انتظار باید حداقل 5 باشند.</p>"
    },
    test_mcnemar_test: {
        isFinal: true,
        name: "آزمون مک‌نمار (McNemar's Test)",
        description: "برای بررسی تغییر در نسبت‌ها برای داده‌های جفت شده (paired categorical data)، معمولاً برای متغیرهای وابسته دوتایی قبل و بعد از یک مداخله.",
        whenToUse: "زمانی که یک متغیر طبقه‌ای دوتایی (binary) دارید که دو بار روی یک گروه از افراد یا واحدهای جفت شده اندازه‌گیری شده است.",
        r_code: `
mcnemar.test(table(data$before_outcome, data$after_outcome))
        `,
        r_code_notes: "<p>این آزمون بر روی سلول‌های خارج از قطر اصلی جدول توافقی تمرکز دارد که نشان‌دهنده تغییر پاسخ‌ها هستند.</p><p>فقط برای داده‌های جفت شده و متغیرهای طبقه‌ای دوتایی مناسب است.</p>"
    },
    test_logistic_regression_simple: {
        isFinal: true,
        name: "رگرسیون لجستیک ساده (Simple Logistic Regression)",
        description: "برای بررسی رابطه بین یک متغیر وابسته دوتایی (binary) و یک متغیر مستقل پیوسته.",
        whenToUse: "زمانی که متغیر وابسته شما دو سطح دارد (مثلاً موفقیت/شکست، بله/خیر) و می‌خواهید تاثیر یک متغیر مستقل پیوسته را بر احتمال وقوع یکی از سطوح متغیر وابسته بررسی کنید.",
        r_code: `
model <- glm(binary_dependent_var ~ continuous_independent_var, data = data, family = binomial(link = "logit"))
summary(model)
        `,
        r_code_notes: "<p><code>family = binomial(link = \"logit\")</code> مشخص می‌کند که از مدل لجستیک استفاده شود.</p><p>خروجی شامل ضرایب (log-odds)، خطاهای استاندارد و p-values است.</p>"
    },
    test_multiple_logistic_regression: {
        isFinal: true,
        name: "رگرسیون لجستیک چندگانه (Multiple Logistic Regression)",
        description: "برای بررسی رابطه بین یک متغیر وابسته دوتایی و دو یا چند متغیر مستقل (پیوسته یا طبقه‌ای).",
        whenToUse: "زمانی که متغیر وابسته شما دو سطح دارد و می‌خواهید تاثیر چندین متغیر مستقل را بر احتمال وقوع یکی از سطوح متغیر وابسته بررسی کنید.",
        r_code: `
model <- glm(binary_dependent_var ~ independent_var1 + independent_var2 + categorical_ind_var, data = data, family = binomial(link = "logit"))
summary(model)
        `,
        r_code_notes: "<p>مشابه رگرسیون لجستیک ساده، اما با چندین متغیر مستقل.</p><p>به هم‌خطی چندگانه بین متغیرهای مستقل توجه کنید.</p>"
    },
    test_logistic_regression_multi_cat_iv: {
        isFinal: true,
        name: "رگرسیون لجستیک (با متغیرهای مستقل طبقه‌ای)",
        description: "برای بررسی رابطه بین یک متغیر وابسته دوتایی و چندین متغیر مستقل که همگی طبقه‌ای هستند.",
        whenToUse: "زمانی که متغیر وابسته شما دو سطح دارد و تمام متغیرهای مستقل شما طبقه‌ای هستند و می‌خواهید تاثیر آن‌ها را بر احتمال وقوع یکی از سطوح متغیر وابسته بررسی کنید.",
        r_code: `
model <- glm(binary_dependent_var ~ cat_iv1 + cat_iv2, data = data, family = binomial(link = "logit"))
summary(model)
        `,
        r_code_notes: "<p>این حالت خاصی از رگرسیون لجستیک چندگانه است که تمام پیش‌بین‌ها طبقه‌ای هستند.</p><p>R به طور خودکار برای متغیرهای فاکتور، متغیرهای ساختگی (dummy variables) ایجاد می‌کند.</p>"
    },
    test_multinomial_logistic_regression: {
        isFinal: true,
        name: "رگرسیون لجستیک چندجمله‌ای (Multinomial Logistic Regression)",
        description: "برای بررسی رابطه بین یک متغیر وابسته طبقه‌ای با بیش از دو سطح اسمی (nominal) و یک یا چند متغیر مستقل.",
        whenToUse: "زمانی که متغیر وابسته شما بیش از دو سطح دارد و این سطوح ترتیب خاصی ندارند (مثلاً انتخاب بین سه برند مختلف).",
        r_code: `
library(nnet)
model <- multinom(multinomial_dv ~ independent_var1 + independent_var2, data = data)
summary(model)
        `,
        r_code_notes: "<p>پکیج <code>nnet</code> (تابع <code>multinom</code>) برای این مدل استفاده می‌شود.</p><p>نتایج به صورت log-odds برای هر سطح از متغیر وابسته نسبت به سطح مرجع ارائه می‌شوند.</p>"
    },
    test_ordinal_logistic_regression: {
        isFinal: true,
        name: "رگرسیون لجستیک ترتیبی (Ordinal Logistic Regression / Proportional Odds Model)",
        description: "برای بررسی رابطه بین یک متغیر وابسته طبقه‌ای ترتیبی (ordinal) و یک یا چند متغیر مستقل.",
        whenToUse: "زمانی که متغیر وابسته شما بیش از دو سطح دارد و این سطوح دارای ترتیب مشخصی هستند (مثلاً سطح رضایت: کم، متوسط، زیاد).",
        r_code: `
library(MASS)
model <- polr(ordinal_dv ~ independent_var1 + independent_var2, data = data, Hess = TRUE)
summary(model)
        `,
        r_code_notes: "<p>پکیج <code>MASS</code> (تابع <code>polr</code>) رایج است.</p><p>این مدل فرض می‌کند که اثر متغیرهای مستقل بر شانس قرار گرفتن در یک سطح یا بالاتر، برای تمام نقاط برش یکسان است.</p>"
    },
    test_ordinal_logistic_regression_multi_iv: {
        isFinal: true,
        name: "رگرسیون لجستیک ترتیبی (با چندین متغیر مستقل)",
        description: "برای بررسی رابطه بین یک متغیر وابسته طبقه‌ای ترتیبی (ordinal) و دو یا چند متغیر مستقل (پیوسته یا طبقه‌ای).",
        whenToUse: "زمانی که متغیر وابسته شما دارای سطوح مرتب است و می‌خواهید تأثیر چندین پیش‌بین را بر آن بررسی کنید.",
        r_code: `
library(MASS)
model <- polr(ordinal_dv ~ independent_var1 + independent_var2 + cat_ind_var, data = data, Hess = TRUE)
summary(model)
        `,
        r_code_notes: "<p>این همان رگرسیون لجستیک ترتیبی است که چندین متغیر مستقل دارد.</p><p>پیش‌فرض proportional odds همچنان پابرجاست و باید بررسی شود.</p>"
    },
    test_log_linear_model_or_mca: {
        isFinal: true,
        name: "مدل‌های لگاریتمی-خطی (Log-linear Models) یا تحلیل تناظر چندگانه (MCA)",
        description: "مدل‌های لگاریتمی-خطی برای تحلیل روابط و همبستگی‌ها در جداول توافقی چندبعدی استفاده می‌شوند. تحلیل تناظر چندگانه (MCA) یک روش کاهش بعد برای داده‌های طبقه‌ای است.",
        whenToUse: "زمانی که بیش از دو متغیر طبقه‌ای دارید و می‌خواهید الگوهای وابستگی بین آن‌ها را بررسی کنید (Log-linear) یا ساختار و روابط بین چندین متغیر طبقه‌ای را با کاهش ابعاد و نمایش بصری بررسی کنید (MCA).",
        r_code: `
library(MASS)
sat_model <- loglm(~ cat_var1*cat_var2*cat_var3, data = my_dataframe_with_factors)
summary(sat_model)
        `,
        r_code_notes: "<p><strong>مدل لگاریتمی-خطی:</strong> برای جداول توافقی با بیش از دو متغیر استفاده می‌شود.</p><p><strong>MCA:</strong> پکیج <code>FactoMineR</code> برای این کار مناسب است.</p>"
    },
    test_poisson_goodness_of_fit: {
        isFinal: true,
        name: "آزمون نیکویی برازش برای توزیع پواسون",
        description: "برای بررسی اینکه آیا داده‌های شمارشی از توزیع پواسون با یک نرخ (λ) مشخص پیروی می‌کنند یا خیر.",
        whenToUse: "زمانی که یک متغیر شمارشی دارید و می‌خواهید بدانید آیا توزیع آن با توزیع پواسون مطابقت دارد.",
        r_code: `
library(vcd)
gf <- goodfit(data$count_variable, type = "poisson", method = "MinChisq")
summary(gf)
        `,
        r_code_notes: "<p>پکیج <code>vcd</code> ابزارهای بهتری برای آزمون نیکویی برازش توزیع‌های شمارشی ارائه می‌دهد.</p>"
    },
    info_descriptive_count: {
        isFinal: true,
        name: "آمار توصیفی برای داده‌های شمارشی",
        description: "توصیف توزیع یک متغیر شمارشی (مانند میانگین، واریانس، هیستوگرام).",
        whenToUse: "زمانی که می‌خواهید ویژگی‌های اصلی یک متغیر شمارشی را خلاصه و نمایش دهید.",
        r_code: `
print(paste("Mean:", mean(data$count_variable)))
print(paste("Variance:", var(data$count_variable)))
print(summary(data$count_variable))
        `,
        r_code_notes: "<p>توابع پایه‌ای R مانند <code>mean()</code>، <code>var()</code>، و <code>summary()</code> برای این کار مفید هستند.</p>"
    },
    test_poisson_regression: {
        isFinal: true,
        name: "رگرسیون پواسون (Poisson Regression)",
        description: "برای مدل‌سازی متغیرهای وابسته شمارشی و بررسی تاثیر متغیرهای مستقل بر نرخ شمارش.",
        whenToUse: "زمانی که متغیر وابسته شما شمارشی است و می‌خواهید تاثیر متغیرهای مستقل را بر آن بررسی کنید.",
        r_code: `
model <- glm(count_dependent_var ~ independent_var1 + independent_var2, data = data, family = poisson(link = "log"))
summary(model)
        `,
        r_code_notes: "<p><code>family = poisson(link = \"log\")</code> مشخص می‌کند که از مدل پواسون استفاده شود.</p><p>اگر پراکندگی بیش از حد وجود داشته باشد، مدل دوجمله‌ای منفی مناسب‌تر است.</p>"
    },
    test_poisson_regression_offset: {
        isFinal: true,
        name: "رگرسیون پواسون با متغیر جبرانی (Offset)",
        description: "برای مدل‌سازی نرخ شمارش زمانی که دوره مشاهده یا میزان 'قرار گرفتن در معرض' برای هر واحد متفاوت است.",
        whenToUse: "زمانی که متغیر وابسته شما شمارشی است و این شمارش‌ها در بازه‌های زمانی متفاوت جمع‌آوری شده‌اند.",
        r_code: `
model <- glm(count_dependent_var ~ independent_var1 + independent_var2 + offset(log(exposure_variable)), data = data, family = poisson(link = "log"))
summary(model)
        `,
        r_code_notes: "<p>متغیر جبرانی با استفاده از <code>offset()</code> و لگاریتم وارد می‌شود.</p><p>بررسی پراکندگی بیش از حد همچنان مهم است.</p>"
    },
    test_negative_binomial_regression: {
        isFinal: true,
        name: "رگرسیون دوجمله‌ای منفی (Negative Binomial Regression)",
        description: "برای مدل‌سازی متغیرهای وابسته شمارشی که پراکندگی بیش از حد (overdispersion) نشان می‌دهند.",
        whenToUse: "زمانی که مدل رگرسیون پواسون به دلیل پراکندگی بیش از حد مناسب نیست.",
        r_code: `
library(MASS)
model <- glm.nb(count_dependent_var ~ independent_var1 + independent_var2, data = data)
summary(model)
        `,
        r_code_notes: "<p>تابع <code>glm.nb()</code> از پکیج <code>MASS</code> برای این مدل استفاده می‌شود.</p><p>این مدل زمانی مناسب است که واریانس به طور قابل توجهی از میانگین بزرگتر باشد.</p>"
    },
    test_negative_binomial_regression_offset: {
        isFinal: true,
        name: "رگرسیون دوجمله‌ای منفی با متغیر جبرانی (Offset)",
        description: "برای مدل‌سازی نرخ شمارش با پراکندگی بیش از حد و با لحاظ کردن دوره مشاهده متفاوت.",
        whenToUse: "زمانی که هم پراکندگی بیش از حد دارید و هم نیاز به متغیر جبرانی دارید.",
        r_code: `
library(MASS)
model <- glm.nb(count_dependent_var ~ independent_var1 + independent_var2 + offset(log(exposure_variable)), data = data)
summary(model)
        `,
        r_code_notes: "<p>ترکیبی از رگرسیون دوجمله‌ای منفی و متغیر جبرانی.</p>"
    },
    test_zero_inflated_poisson_or_nb: {
        isFinal: true,
        name: "مدل‌های شمارشی با تورم صفر (Zero-Inflated Poisson/Negative Binomial)",
        description: "برای مدل‌سازی داده‌های شمارشی که تعداد زیادی صفر بیش از حد انتظار دارند.",
        whenToUse: "زمانی که تعداد صفرها در متغیر وابسته شمارشی شما بیشتر از حد انتظار باشد.",
        r_code: `
library(pscl)
model_example_zinb <- zeroinfl(count_dependent_var ~ var1 | var1, data = data, dist = "negbin")
summary(model_example_zinb)
        `,
        r_code_notes: "<p>پکیج <code>pscl</code> (تابع <code>zeroinfl</code>) برای این مدل‌ها استفاده می‌شود.</p><p>انتخاب بین ZIP و ZINB بستگی به پراکندگی دارد.</p>"
    }
};

let currentNodeKey = 'start'; // شروع از گره 'start'

function displayNode(nodeKey) {
    const node = decisionTree[nodeKey];
    if (!node) {
        questionTextElement.textContent = "خطا: گره تعریف نشده است. لطفاً به توسعه‌دهنده اطلاع دهید.";
        optionsContainerElement.innerHTML = "";
        return;
    }

    currentNodeKey = nodeKey; // ذخیره گره فعلی

    if (node.isFinal) {
        questionContainerElement.classList.add('hidden');
        resultContainerElement.classList.remove('hidden');
        resetButtonElement.classList.remove('hidden');

        testNameElement.textContent = node.name || "نام آزمون در دسترس نیست";
        testDescriptionElement.innerHTML = node.description ? node.description.replace(/\n/g, '<br>') : "توضیحات در دسترس نیست.";
        testWhenToUseElement.innerHTML = node.whenToUse ? node.whenToUse.replace(/\n/g, '<br>') : "اطلاعاتی در مورد زمان استفاده موجود نیست.";
        
        rCodeElement.textContent = node.r_code ? node.r_code.trim() : "کد R نمونه موجود نیست.";
        rCodeNotesElement.innerHTML = node.r_code_notes || "";
    } else {
        questionContainerElement.classList.remove('hidden');
        resultContainerElement.classList.add('hidden');
        resetButtonElement.classList.add('hidden');

        questionTextElement.textContent = node.question;
        optionsContainerElement.innerHTML = "";

        node.options.forEach(option => {
            const button = document.createElement('button');
            button.textContent = option.text;
            button.onclick = () => displayNode(option.nextNode);
            optionsContainerElement.appendChild(button);
        });
    }
}

function resetGuide() {
    currentNodeKey = 'start';
    displayNode('start');
}

function copyCode() {
    const codeToCopy = rCodeElement.innerText;
    navigator.clipboard.writeText(codeToCopy).then(() => {
        alert('کد R در کلیپ‌بورد کپی شد!');
    }).catch(err => {
        console.error('خطا در کپی کردن کد: ', err);
        const textArea = document.createElement('textarea');
        textArea.value = codeToCopy;
        document.body.appendChild(textArea);
        textArea.select();
        document.execCommand('copy');
        document.body.removeChild(textArea);
        alert('کد R در کلیپ‌بورد کپی شد (روش جایگزین)!');
    });
}

// Event Listeners
resetButtonElement.addEventListener('click', resetGuide);

// --- شروع برنامه ---
document.addEventListener('DOMContentLoaded', () => {
    if (!questionTextElement || !optionsContainerElement || !resultContainerElement) {
        console.error('یکی از المنت‌های DOM پیدا نشد. مطمئن شوید IDها در HTML درست تعریف شده‌اند.');
        return;
    }
    displayNode('start');
    if (typeof Prism !== 'undefined') {
        Prism.highlightElement(rCodeElement);
    }
});
