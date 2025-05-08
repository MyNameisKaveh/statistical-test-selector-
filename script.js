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
// بر اساس: https://stats.oarc.ucla.edu/other/mult-pkg/whatstat/
const decisionTree = {
    start: {
        question: "نوع متغیر وابسته اصلی (Dependent Variable) شما چیست؟",
        options: [
            { text: "پیوسته (Interval / Ratio)", nextNode: "q_dv_continuous_num_dvs" },
            { text: "طبقه‌ای / دسته‌ای (Categorical: Nominal / Ordinal)", nextNode: "q_dv_categorical_num_dvs" },
            { text: "شمارشی (Count)", nextNode: "q_dv_count_num_dvs" }
        ]
    },

    // --- مسیر متغیر وابسته پیوسته ---
    q_dv_continuous_num_dvs: {
        question: "چند متغیر وابسته پیوسته دارید؟",
        options: [
            { text: "یک متغیر وابسته پیوسته", nextNode: "q_one_dv_continuous_num_ivs" },
            { text: "دو یا چند متغیر وابسته پیوسته", nextNode: "q_multi_dv_continuous_num_ivs" }
        ]
    },

    // --- یک متغیر وابسته پیوسته ---
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
            // می‌توان آزمون‌های توصیفی دیگر را نیز افزود
        ]
    },

    // --- یک متغیر وابسته پیوسته، یک متغیر مستقل ---
    q_one_dv_one_iv_type: {
        question: "نوع متغیر مستقل شما چیست؟",
        options: [
            { text: "طبقه‌ای با دو سطح/گروه (مثلاً زن/مرد، درمان/کنترل)", nextNode: "q_one_dv_one_iv_cat2_paired" },
            { text: "طبقه‌ای با سه یا چند سطح/گروه (مثلاً سه روش درمانی مختلف)", nextNode: "q_one_dv_one_iv_cat_multi_paired" },
            { text: "پیوسته (Interval / Ratio)", nextNode: "test_simple_linear_regression" }
            // { text: "ترتیبی (Ordinal)", nextNode: "info_ordinal_iv_treat_as_cat_or_cont" } // نیاز به تصمیم گیری کاربر
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

    // --- یک متغیر وابسته پیوسته، دو یا چند متغیر مستقل ---
    q_one_dv_multi_iv_type: {
        question: "ماهیت متغیرهای مستقل شما چگونه است؟",
        options: [
            { text: "همگی طبقه‌ای (Categorical)", nextNode: "q_one_dv_multi_iv_all_cat_interaction" },
            { text: "همگی پیوسته (Continuous)", nextNode: "test_multiple_regression" },
            { text: "ترکیبی از پیوسته و طبقه‌ای", nextNode: "test_ancova_or_regression_with_dummies" } // ANCOVA is a common case
        ]
    },
    q_one_dv_multi_iv_all_cat_interaction: {
        question: "آیا علاقه‌مند به بررسی اثرات متقابل (interaction effects) بین متغیرهای مستقل طبقه‌ای هستید؟",
        options: [
            { text: "بله، می‌خواهم اثرات متقابل را بررسی کنم", nextNode: "q_one_dv_multi_iv_all_cat_paired_interaction" }, // Leads to Factorial ANOVA
            { text: "خیر، فقط اثرات اصلی (main effects) مد نظر است", nextNode: "q_one_dv_multi_iv_all_cat_paired_no_interaction" } // Leads to ANOVA with main effects
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
     q_one_dv_multi_iv_all_cat_paired_no_interaction: { // Simplified path if no interaction
        question: "آیا داده‌ها شامل اندازه‌گیری‌های مکرر (repeated measures) روی حداقل یکی از فاکتورها هستند؟",
        options: [
            { text: "خیر، تمام فاکتورها بین آزمودنی (between-subjects) هستند.", nextNode: "test_anova_main_effects_only_independent" }, // Could be Factorial ANOVA without interaction term
            { text: "بله، حداقل یک فاکتور درون آزمودنی (within-subjects / repeated measures) است.", nextNode: "test_repeated_measures_anova_main_effects_only" } 
        ]
    },


    // --- دو یا چند متغیر وابسته پیوسته ---
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
            { text: "طبقه‌ای (Categorical)", nextNode: "test_manova" }, // Typically MANOVA for one categorical IV
            { text: "پیوسته (Continuous)", nextNode: "test_multivariate_multiple_regression" } // Or could be canonical correlation if IVs also multiple
        ]
    },
    q_multi_dv_multi_iv_type: {
        question: "ماهیت متغیرهای مستقل شما چگونه است؟",
        options: [
            { text: "همگی طبقه‌ای (Categorical)", nextNode: "test_factorial_manova" },
            { text: "همگی پیوسته (Continuous) یا ترکیبی", nextNode: "test_multivariate_multiple_regression" }
        ]
    },

    // --- مسیر متغیر وابسته طبقه‌ای ---
    q_dv_categorical_num_dvs: {
        question: "چند متغیر وابسته طبقه‌ای دارید؟",
        options: [
            { text: "یک متغیر وابسته طبقه‌ای", nextNode: "q_one_dv_categorical_num_ivs" },
            { text: "دو یا چند متغیر وابسته طبقه‌ای", nextNode: "test_log_linear_model_or_mca" } // More complex, Log-linear for associations, MCA for structure
        ]
    },

    // --- یک متغیر وابسته طبقه‌ای ---
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

    // --- یک متغیر وابسته طبقه‌ای، یک متغیر مستقل ---
    q_one_dv_one_iv_cat_type_of_iv: {
        question: "نوع متغیر مستقل شما چیست؟",
        options: [
            { text: "طبقه‌ای (Categorical)", nextNode: "q_one_dv_cat_one_iv_cat_paired" },
            { text: "پیوسته (Continuous)", nextNode: "test_logistic_regression_simple" }, // Assuming binary DV for simplicity, could be multinomial
            { text: "ترتیبی (Ordinal)", nextNode: "test_ordinal_logistic_regression" }
        ]
    },
    q_one_dv_cat_one_iv_cat_paired: {
        question: "آیا مشاهدات/گروه‌ها مستقل هستند یا جفت شده/وابسته؟",
        options: [
            { text: "مستقل (Independent samples)", nextNode: "test_chi_square_test_of_independence" }, // Or Fisher's Exact Test for small samples
            { text: "جفت شده / وابسته (Paired samples / Repeated measures)", nextNode: "test_mcnemar_test" } // For binary DV, repeated on same subject
        ]
    },

    // --- یک متغیر وابسته طبقه‌ای، دو یا چند متغیر مستقل ---
    q_one_dv_multi_iv_cat_type_of_ivs: {
        question: "ماهیت متغیرهای مستقل شما چگونه است؟ (نوع متغیر وابسته طبقه‌ای شما چیست؟ دو سطحی یا چند سطحی؟)",
        options: [
            // Assuming DV is binary for simplicity in Logistic Regression path
            { text: "متغیرهای مستقل همگی طبقه‌ای (DV دو سطحی)", nextNode: "test_logistic_regression_multi_cat_iv" }, // Can also use log-linear if interested in associations
            { text: "متغیرهای مستقل همگی پیوسته یا ترکیبی (DV دو سطحی)", nextNode: "test_multiple_logistic_regression" },
            { text: "متغیر وابسته چند سطحی اسمی (Nominal)", nextNode: "test_multinomial_logistic_regression" },
            { text: "متغیر وابسته چند سطحی ترتیبی (Ordinal)", nextNode: "test_ordinal_logistic_regression_multi_iv" }
        ]
    },

    // --- مسیر متغیر وابسته شمارشی (Count) ---
    q_dv_count_num_dvs: { // Assuming one count DV for simplicity, common case
        question: "چند متغیر مستقل (Independent Variable) اصلی دارید؟",
        options: [
            { text: "هیچ (مقایسه میانگین شمارش با یک مقدار ثابت یا بررسی توزیع)", nextNode: "q_one_dv_count_no_iv_goal" },
            { text: "یک یا چند متغیر مستقل (برای مدل‌سازی نرخ شمارش)", nextNode: "q_one_dv_count_iv_exposure" }
        ]
    },
    q_one_dv_count_no_iv_goal: {
        question: "هدف شما چیست؟",
        options: [
            { text: "مقایسه نرخ شمارش مشاهده شده با یک نرخ مورد انتظار", nextNode: "test_poisson_goodness_of_fit" }, // Can be a chi-square variant
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
    q_one_dv_count_iv_dispersion: { // Path without explicit offset
        question: "آیا پراکندگی بیش از حد (Overdispersion: واریانس بزرگتر از میانگین) در داده‌های شمارشی شما محتمل است؟",
        options: [
            { text: "خیر، یا مطمئن نیستم (شروع با پواسون).", nextNode: "test_poisson_regression" },
            { text: "بله، پراکندگی بیش از حد وجود دارد یا محتمل است.", nextNode: "test_negative_binomial_regression" },
            { text: "داده‌ها شامل تعداد زیادی صفر هستند (Zero-inflated).", nextNode: "test_zero_inflated_poisson_or_nb" }
        ]
    },
    q_one_dv_count_iv_dispersion_offset: { // Path with explicit offset
        question: "آیا پراکندگی بیش از حد (Overdispersion) در داده‌های شمارشی شما محتمل است؟ (با لحاظ کردن متغیر جبرانی)",
        options: [
            { text: "خیر، یا مطمئن نیستم (شروع با پواسون با offset).", nextNode: "test_poisson_regression_offset" },
            { text: "بله، پراکندگی بیش از حد وجود دارد یا محتمل است (با offset).", nextNode: "test_negative_binomial_regression_offset" }
        ]
    },


    // --- تعریف آزمون‌های نهایی (Final Tests) ---
    
    // ## آزمون‌های مربوط به یک متغیر وابسته پیوسته ##
    test_one_sample_t_test: {
        isFinal: true,
        name: "آزمون تی تک نمونه‌ای (One-Sample t-test)",
        description: "برای مقایسه میانگین یک گروه با یک مقدار مشخص (مقدار فرضی یا میانگین جامعه).",
        whenToUse: "زمانی که یک متغیر وابسته پیوسته دارید و می‌خواهید بدانید آیا میانگین آن در نمونه شما به طور معناداری با یک مقدار ثابت (μ0) تفاوت دارد یا خیر. داده‌ها باید تقریباً نرمال توزیع شده باشند.",
        r_code: `
# H0: a میانگین نمونه برابر با mu0 است
# HA: میانگین نمونه برابر با mu0 نیست
# data$variable متغیر پیوسته شما
# mu0 مقدار مشخصی است که می‌خواهید میانگین را با آن مقایسه کنید
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
# H0: داده‌ها از توزیع نرمال پیروی می‌کنند
# HA: داده‌ها از توزیع نرمال پیروی نمی‌کنند
# data$variable متغیر پیوسته شما
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
# H0: میانگین دو گروه برابر است
# HA: میانگین دو گروه برابر نیست
# data$dependent_var متغیر وابسته پیوسته
# data$group_var متغیر مستقل طبقه‌ای با دو سطح (مثلاً "A", "B")
t.test(dependent_var ~ group_var, data = data, var.equal = FALSE) # Welch t-test (پیش‌فرض)
# t.test(dependent_var ~ group_var, data = data, var.equal = TRUE) # Student's t-test (اگر واریانس‌ها برابرند)
        `,
        r_code_notes: "<p><code>var.equal = FALSE</code> (پیش‌فرض در R) آزمون ولچ را اجرا می‌کند که در صورت نابرابری واریانس‌ها معتبرتر است.</p><p>برای بررسی برابری واریانس‌ها می‌توانید از آزمون Levene (<code>car::leveneTest()</code>) استفاده کنید.</p>"
    },
    test_paired_samples_t_test: {
        isFinal: true,
        name: "آزمون تی نمونه‌های جفت شده (Paired Samples t-test)",
        description: "برای مقایسه میانگین‌های دو اندازه‌گیری وابسته (جفت شده) روی یک گروه از آزمودنی‌ها.",
        whenToUse: "زمانی که یک متغیر وابسته پیوسته دارید که دو بار روی یک گروه از افراد یا واحدهای جفت شده اندازه‌گیری شده است (مثلاً قبل و بعد از یک مداخله). پیش‌فرض این است که تفاوت بین جفت‌ها نرمال توزیع شده باشد.",
        r_code: `
# H0: میانگین تفاوت بین جفت‌ها صفر است
# HA: میانگین تفاوت بین جفت‌ها صفر نیست
# data$measurement1 اولین اندازه‌گیری
# data$measurement2 دومین اندازه‌گیری
t.test(data$measurement1, data$measurement2, paired = TRUE)

# یا اگر داده‌ها در فرمت long هستند:
# t.test(value ~ time_variable, data = long_data, paired = TRUE, subset = id_variable %in% some_ids)
        `,
        r_code_notes: "<p><code>paired = TRUE</code> مشخص می‌کند که آزمون برای نمونه‌های جفت شده است.</p><p>اطمینان حاصل کنید که ترتیب دو متغیر در تابع <code>t.test</code> درست است، به خصوص اگر جهت تفاوت برایتان مهم است.</p>"
    },
    test_simple_linear_regression: {
        isFinal: true,
        name: "رگرسیون خطی ساده (Simple Linear Regression)",
        description: "برای بررسی رابطه خطی بین یک متغیر وابسته پیوسته و یک متغیر مستقل پیوسته.",
        whenToUse: "زمانی که می‌خواهید پیش‌بینی کنید یا توضیح دهید که چگونه یک متغیر مستقل پیوسته بر یک متغیر وابسته پیوسته تاثیر می‌گذارد.",
        r_code: `
# data$dependent_var متغیر وابسته پیوسته
# data$independent_var متغیر مستقل پیوسته
model <- lm(dependent_var ~ independent_var, data = data)
summary(model)
# برای مشاهده نمودار پراکندگی با خط رگرسیون:
# plot(data$independent_var, data$dependent_var)
# abline(model)
        `,
        r_code_notes: "<p><code>lm</code> مخفف linear model است.</p><p><code>summary(model)</code> ضرایب، خطای استاندارد، آماره t، p-value و R-squared را نشان می‌دهد.</p><p>پیش‌فرض‌ها شامل خطی بودن رابطه، استقلال خطاها، همسانی واریانس خطاها (homoscedasticity) و نرمال بودن توزیع خطاها است.</p>"
    },
    test_one_way_anova: {
        isFinal: true,
        name: "تحلیل واریانس یک‌طرفه (One-Way ANOVA)",
        description: "برای مقایسه میانگین‌های سه یا چند گروه مستقل.",
        whenToUse: "زمانی که یک متغیر وابسته پیوسته و یک متغیر مستقل طبقه‌ای با سه یا چند سطح (گروه مستقل) دارید. پیش‌فرض‌ها شامل نرمال بودن داده‌ها در هر گروه، برابری واریانس‌ها بین گروه‌ها (homoscedasticity) و استقلال مشاهدات است.",
        r_code: `
# data$dependent_var متغیر وابسته پیوسته
# data$group_var متغیر مستقل طبقه‌ای (فاکتور) با k سطح (k >= 3)
# group_var باید از نوع factor باشد. اگر نیست: data$group_var <- factor(data$group_var)
model <- aov(dependent_var ~ group_var, data = data)
summary(model)

# برای آزمون‌های تعقیبی (Post-hoc tests) اگر ANOVA معنادار بود:
# TukeyHSD(model)
        `,
        r_code_notes: "<p>اگر متغیر گروه شما عددی است، ابتدا آن را به <code>factor</code> تبدیل کنید.</p><p>اگر نتیجه ANOVA معنادار بود (p-value کوچک)، از آزمون‌های تعقیبی مانند Tukey HSD برای مقایسه جفتی میانگین‌ها استفاده کنید.</p><p>برای بررسی برابری واریانس‌ها از آزمون Levene (<code>car::leveneTest()</code>) استفاده کنید.</p>"
    },
    test_one_way_repeated_measures_anova: {
        isFinal: true,
        name: "تحلیل واریانس یک‌طرفه با اندازه‌گیری‌های مکرر (One-Way Repeated Measures ANOVA)",
        description: "برای مقایسه میانگین‌ها در سه یا چند سطح از یک فاکتور درون‌آزمودنی (within-subjects factor).",
        whenToUse: "زمانی که یک متغیر وابسته پیوسته دارید که سه بار یا بیشتر روی یک گروه از افراد یا واحدهای مشابه اندازه‌گیری شده است (مثلاً در سه نقطه زمانی مختلف). پیش‌فرض کرویت (sphericity) مهم است.",
        r_code: `
# نیاز به پکیج rstatix یا afex یا car برای اجرای راحت‌تر و بررسی کرویت
# فرض کنید داده‌ها در فرمت long هستند:
# data_long شامل ستون‌های: id (شناسه آزمودنی), time (فاکتور درون‌آزمودنی), value (متغیر وابسته)
# install.packages("rstatix")
library(rstatix)

# data_long$id <- factor(data_long$id)
# data_long$time <- factor(data_long$time)

model <- anova_test(data = data_long, 
                    dv = value, 
                    wid = id, 
                    within = time)
print(model)

# اگر کرویت نقض شود (Mauchly's test p < 0.05), از تصحیحات (Greenhouse-Geisser or Huynh-Feldt) استفاده کنید.
# rstatix به طور خودکار این کار را انجام می‌دهد و گزارش می‌کند.
        `,
        r_code_notes: "<p>داده‌ها باید در فرمت 'long' باشند: هر ردیف یک مشاهده، با ستون‌هایی برای شناسه فرد، فاکتور درون‌آزمودنی و مقدار متغیر وابسته.</p><p>پیش‌فرض کرویت (sphericity) با آزمون ماچلی (Mauchly's Test) بررسی می‌شود. اگر نقض شود، از مقادیر p تصحیح شده (مانند Greenhouse-Geisser) استفاده کنید.</p><p>پکیج‌های <code>rstatix</code>، <code>afex</code> یا <code>ez</code> اجرای این آزمون را ساده‌تر می‌کنند.</p>"
    },
    test_multiple_regression: {
        isFinal: true,
        name: "رگرسیون چندگانه (Multiple Regression)",
        description: "برای بررسی رابطه بین یک متغیر وابسته پیوسته و دو یا چند متغیر مستقل (پیوسته یا طبقه‌ای).",
        whenToUse: "زمانی که می‌خواهید پیش‌بینی کنید یا توضیح دهید که چگونه چندین متغیر مستقل بر یک متغیر وابسته پیوسته تاثیر می‌گذارند. متغیرهای مستقل طبقه‌ای باید به صورت متغیرهای ساختگی (dummy variables) وارد شوند.",
        r_code: `
# data$dependent_var متغیر وابسته پیوسته
# data$independent_var1, data$independent_var2, ... متغیرهای مستقل
# اگر متغیر مستقل طبقه‌ای دارید، R به طور خودکار آن‌ها را به dummy تبدیل می‌کند اگر از نوع factor باشند.
model <- lm(dependent_var ~ independent_var1 + independent_var2 + independent_var_categorical, data = data)
summary(model)
# برای بررسی هم‌خطی چندگانه (Multicollinearity):
# library(car)
# vif(model)
        `,
        r_code_notes: "<p>پیش‌فرض‌ها مشابه رگرسیون ساده است اما شامل عدم هم‌خطی چندگانه شدید بین متغیرهای مستقل نیز می‌شود (با VIF بررسی کنید).</p><p>اگر متغیرهای مستقل طبقه‌ای دارید، مطمئن شوید که در R به عنوان <code>factor</code> تعریف شده‌اند.</p>"
    },
    test_ancova_or_regression_with_dummies: { // Broad category, ANCOVA is a specific case
        isFinal: true,
        name: "رگرسیون با متغیرهای مستقل پیوسته و طبقه‌ای (ANCOVA به عنوان حالت خاص)",
        description: "برای بررسی تاثیر متغیرهای مستقل طبقه‌ای و پیوسته بر یک متغیر وابسته پیوسته. ANCOVA به طور خاص به مقایسه میانگین‌های تعدیل شده گروه‌ها پس از کنترل اثر یک یا چند متغیر پیوسته (covariate) می‌پردازد.",
        whenToUse: "زمانی که می‌خواهید اثر یک یا چند متغیر مستقل طبقه‌ای را بر متغیر وابسته پیوسته بررسی کنید، در حالی که اثر یک یا چند متغیر مستقل پیوسته (covariates) را کنترل می‌کنید. یا به طور کلی، مدلی با ترکیبی از انواع متغیرهای مستقل دارید.",
        r_code: `
# ANCOVA مثال: dependent_var ~ covariate_continuous + group_factor
# data$dependent_var متغیر وابسته پیوسته
# data$covariate_continuous متغیر مستقل پیوسته (همبسته)
# data$group_factor متغیر مستقل طبقه‌ای (فاکتور)
# group_factor باید factor باشد.
# data$group_factor <- factor(data$group_factor)

model_ancova <- lm(dependent_var ~ covariate_continuous + group_factor, data = data)
summary(model_ancova)
# برای دیدن جدول ANOVA (Type II or III recommended for unbalanced designs or interactions)
# library(car)
# Anova(model_ancova, type="III")

# اگر اثر متقابل (interaction) بین همبسته و فاکتور مهم است:
# model_interaction <- lm(dependent_var ~ covariate_continuous * group_factor, data = data)
# summary(model_interaction)
# Anova(model_interaction, type="III")
        `,
        r_code_notes: "<p>در ANCOVA، یک پیش‌فرض مهم، همگنی شیب‌های رگرسیون (homogeneity of regression slopes) است، یعنی عدم وجود اثر متقابل معنادار بین همبسته و فاکتور گروه‌بندی. این را با وارد کردن جمله <code>covariate_continuous * group_factor</code> در مدل بررسی کنید.</p><p>اگر همبستگی زیادی بین همبسته(ها) و متغیرهای مستقل طبقه‌ای وجود داشته باشد، تفسیر نتایج دشوار می‌شود.</p>"
    },
    test_factorial_anova_independent: {
        isFinal: true,
        name: "تحلیل واریانس عاملی (بین آزمودنی‌ها) (Factorial ANOVA - Independent Groups)",
        description: "برای بررسی اثرات اصلی و متقابل دو یا چند متغیر مستقل طبقه‌ای (فاکتور) بر یک متغیر وابسته پیوسته، زمانی که گروه‌ها مستقل هستند.",
        whenToUse: "زمانی که بیش از یک متغیر مستقل طبقه‌ای دارید و می‌خواهید تاثیر هر یک به تنهایی (اثر اصلی) و تاثیر ترکیب آن‌ها (اثر متقابل) را بر متغیر وابسته پیوسته بررسی کنید. تمام فاکتورها بین آزمودنی هستند (هر آزمودنی فقط در یک ترکیب از سطوح فاکتورها قرار دارد).",
        r_code: `
# data$dependent_var متغیر وابسته پیوسته
# data$factorA, data$factorB متغیرهای مستقل طبقه‌ای (فاکتورها)
# این فاکتورها باید از نوع factor باشند.
# data$factorA <- factor(data$factorA)
# data$factorB <- factor(data$factorB)

# مدل با اثرات اصلی و متقابل
model <- aov(dependent_var ~ factorA * factorB, data = data)
summary(model)

# برای مدل فقط با اثرات اصلی (اگر مطمئن هستید اثر متقابل وجود ندارد یا مهم نیست):
# model_main_effects <- aov(dependent_var ~ factorA + factorB, data = data)
# summary(model_main_effects)

# برای آزمون‌های تعقیبی در صورت وجود اثرات معنادار، از پکیج‌های emmeans یا phia استفاده کنید.
# library(emmeans)
# emmeans(model, specs = pairwise ~ factorA | factorB) # مقایسه سطوح factorA در هر سطح از factorB
        `,
        r_code_notes: "<p><code>factorA * factorB</code> شامل اثرات اصلی factorA، factorB و اثر متقابل factorA:factorB می‌شود.</p><p>پیش‌فرض‌ها مشابه ANOVA یک‌طرفه (نرمال بودن، همسانی واریانس‌ها، استقلال مشاهدات).</p><p>تفسیر اثرات اصلی در حضور اثر متقابل معنادار باید با احتیاط انجام شود.</p>"
    },
    test_factorial_anova_repeated_measures: {
        isFinal: true,
        name: "تحلیل واریانس عاملی با اندازه‌گیری‌های مکرر (Factorial Repeated Measures ANOVA)",
        description: "برای بررسی اثرات اصلی و متقابل دو یا چند فاکتور درون‌آزمودنی (within-subjects factors) بر یک متغیر وابسته پیوسته.",
        whenToUse: "زمانی که بیش از یک فاکتور دارید که همگی به صورت مکرر روی یک گروه از آزمودنی‌ها اندازه‌گیری شده‌اند (مثلاً عملکرد در شرایط مختلف A و در زمان‌های مختلف B).",
        r_code: `
# نیاز به پکیج rstatix یا afex. داده‌ها باید در فرمت long باشند.
# data_long: id, within_factor1, within_factor2, dependent_var
# install.packages("rstatix")
library(rstatix)

# data_long$id <- factor(data_long$id)
# data_long$within_factor1 <- factor(data_long$within_factor1)
# data_long$within_factor2 <- factor(data_long$within_factor2)

model <- anova_test(
  data = data_long,
  dv = dependent_var,
  wid = id,
  within = c(within_factor1, within_factor2)
)
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
# نیاز به پکیج rstatix یا afex. داده‌ها باید در فرمت long باشند.
# data_long: id, between_factor, within_factor, dependent_var
# install.packages("rstatix")
library(rstatix)

# data_long$id <- factor(data_long$id)
# data_long$between_factor <- factor(data_long$between_factor)
# data_long$within_factor <- factor(data_long$within_factor)

model <- anova_test(
  data = data_long,
  dv = dependent_var,
  wid = id,
  between = between_factor,
  within = within_factor
)
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
# data$dependent_var متغیر وابسته پیوسته
# data$factorA, data$factorB متغیرهای مستقل طبقه‌ای (فاکتورها)
# data$factorA <- factor(data$factorA)
# data$factorB <- factor(data$factorB)

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
# نیاز به پکیج rstatix یا afex. داده‌ها باید در فرمت long باشند.
# data_long: id, within_factor1, within_factor2, dependent_var
library(rstatix)

# data_long$id <- factor(data_long$id)
# data_long$within_factor1 <- factor(data_long$within_factor1)
# data_long$within_factor2 <- factor(data_long$within_factor2)

# توجه: تابع anova_test در rstatix به طور پیش‌فرض اثر متقابل را هم محاسبه می‌کند.
# برای مدل فقط با اثرات اصلی، باید فرمول را دستی در توابعی مانند lme (از nlme) یا lmer (از lme4) مشخص کرد
# که پیچیده‌تر است. یک راه ساده‌تر (اما کمتر دقیق) اجرای مدل‌های جداگانه One-Way RM ANOVA برای هر فاکتور است،
# اما این کار اثرات دیگر فاکتورها را کنترل نمی‌کند.

# مثال مفهومی با anova_test (که اثر متقابل هم می‌دهد):
model <- anova_test(
  data = data_long,
  dv = dependent_var,
  wid = id,
  within = list(within_factor1, within_factor2) # برای جدا کردن اثرات
)
# سپس فقط به ردیف‌های مربوط به اثرات اصلی در خروجی توجه کنید.
# برای مدل دقیق‌تر بدون اثر متقابل، جستجو کنید: "main effects only repeated measures anova in R"
        `,
        r_code_notes: "<p>اجرای یک مدل RM ANOVA که فقط اثرات اصلی را بدون اثرات متقابل ارزیابی کند، می‌تواند کمی پیچیده‌تر باشد و اغلب نیاز به استفاده از مدل‌های خطی مختلط (mixed models) دارد، به خصوص اگر طرح نامتعادل باشد یا پیش‌فرض‌ها نقض شوند.</p><p>روش ارائه شده در کد، ساده‌ترین راه با `rstatix` است که تمام اثرات (از جمله متقابل) را گزارش می‌دهد و شما باید روی اثرات اصلی تمرکز کنید.</p>"
    },


    // ## آزمون‌های مربوط به چندین متغیر وابسته پیوسته ##
    test_manova: {
        isFinal: true,
        name: "تحلیل واریانس چندمتغیره (MANOVA)",
        description: "برای مقایسه میانگین‌های چندین متغیر وابسته پیوسته بین دو یا چند گروه مستقل (تعریف شده توسط یک متغیر مستقل طبقه‌ای).",
        whenToUse: "زمانی که بیش از یک متغیر وابسته پیوسته دارید و می‌خواهید تاثیر یک متغیر مستقل طبقه‌ای را بر ترکیب خطی این متغیرهای وابسته بررسی کنید.",
        r_code: `
# data$Y1, data$Y2, ... متغیرهای وابسته پیوسته
# data$Group متغیر مستقل طبقه‌ای (فاکتور)
# data$Group <- factor(data$Group)

# ترکیب متغیرهای وابسته در یک ماتریس
dependent_vars_matrix <- cbind(data$Y1, data$Y2) # اضافه کردن سایر Y ها در صورت وجود

model_manova <- manova(dependent_vars_matrix ~ Group, data = data)
summary(model_manova, test = "Pillai") # سایر آماره‌ها: "Wilks", "Hotelling-Lawley", "Roy"

# اگر MANOVA معنادار بود، برای بررسی تاثیر بر هر DV به صورت جداگانه می‌توان ANOVAهای جداگانه اجرا کرد (با احتیاط و تصحیح بونفرونی)
# summary.aov(model_manova) # نتایج ANOVAهای جداگانه برای هر DV
        `,
        r_code_notes: "<p><code>cbind()</code> برای ایجاد ماتریس متغیرهای وابسته استفاده می‌شود.</p><p>آماره Pillai's Trace اغلب به عنوان یک آماره قوی (robust) توصیه می‌شود.</p><p>پیش‌فرض‌ها شامل نرمال بودن چندمتغیره متغیرهای وابسته در هر گروه و همسانی ماتریس‌های کوواریانس بین گروه‌ها (با آزمون Box's M بررسی می‌شود) است.</p>"
    },
    test_factorial_manova: {
        isFinal: true,
        name: "تحلیل واریانس چندمتغیره عاملی (Factorial MANOVA)",
        description: "برای بررسی اثرات اصلی و متقابل دو یا چند متغیر مستقل طبقه‌ای بر چندین متغیر وابسته پیوسته.",
        whenToUse: "زمانی که بیش از یک متغیر وابسته پیوسته و بیش از یک متغیر مستقل طبقه‌ای دارید و می‌خواهید اثرات اصلی و متقابل فاکتورها را بر ترکیب خطی متغیرهای وابسته بررسی کنید.",
        r_code: `
# data$Y1, data$Y2, ... متغیرهای وابسته پیوسته
# data$FactorA, data$FactorB متغیرهای مستقل طبقه‌ای
# data$FactorA <- factor(data$FactorA)
# data$FactorB <- factor(data$FactorB)

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
# data$Y1, data$Y2, ... متغیرهای وابسته پیوسته
# data$X1, data$X2, ... متغیرهای مستقل (پیوسته یا طبقه‌ای فاکتور شده)

dependent_vars_matrix <- cbind(data$Y1, data$Y2)

# این در واقع یک lm() است که با ماتریس Y اجرا می‌شود
model_mv_regr <- lm(dependent_vars_matrix ~ X1 + X2, data = data)
summary(model_mv_regr) # خلاصه جداگانه برای هر Y می‌دهد

# برای آماره‌های چندمتغیره کلی (مانند Wilks' Lambda) می‌توان از manova() استفاده کرد:
# summary(manova(dependent_vars_matrix ~ X1 + X2, data = data))
        `,
        r_code_notes: "<p>تابع <code>lm()</code> در R می‌تواند یک ماتریس از متغیرهای وابسته را بپذیرد.</p><p>خروجی <code>summary()</code> نتایج رگرسیون را برای هر متغیر وابسته به طور جداگانه نشان می‌دهد.</p><p>برای ارزیابی کلی مدل و معناداری چندمتغیره متغیرهای مستقل، می‌توانید از تابع <code>manova()</code> با همان فرمول استفاده کنید.</p>"
    },

    // ## آزمون‌های مربوط به یک متغیر وابسته طبقه‌ای ##
    test_chi_square_goodness_of_fit: {
        isFinal: true,
        name: "آزمون خی‌دو نیکویی برازش (Chi-squared Goodness-of-Fit Test)",
        description: "برای مقایسه فراوانی‌های مشاهده شده یک متغیر طبقه‌ای با فراوانی‌های مورد انتظار (نظری).",
        whenToUse: "زمانی که یک متغیر طبقه‌ای دارید و می‌خواهید بدانید آیا توزیع فراوانی آن در نمونه شما با یک توزیع مشخص (مثلاً توزیع یکنواخت یا توزیع مشخص شده از قبل) مطابقت دارد یا خیر.",
        r_code: `
# data$categorical_var متغیر طبقه‌ای شما
observed_frequencies <- table(data$categorical_var)

# مثال: اگر انتظار توزیع یکنواخت دارید
# n_categories <- length(observed_frequencies)
# expected_probabilities <- rep(1/n_categories, n_categories)

# مثال: اگر احتمالات مورد انتظار مشخصی دارید (باید جمعشان 1 شود)
# expected_probabilities <- c(0.25, 0.50, 0.25) # برای 3 سطح

# اطمینان از اینکه طول expected_probabilities با تعداد سطوح مشاهده شده یکی است
if (length(observed_frequencies) != length(expected_probabilities)) {
  stop("تعداد سطوح مشاهده شده با احتمالات مورد انتظار همخوانی ندارد.")
}

chisq.test(x = observed_frequencies, p = expected_probabilities)
        `,
        r_code_notes: "<p><code>observed_frequencies</code> یک جدول از فراوانی‌های سطوح متغیر طبقه‌ای شماست.</p><p><code>p</code> یک بردار از احتمالات مورد انتظار برای هر سطح است (باید جمعشان 1 شود).</p><p>پیش‌فرض: تمام فراوانی‌های مورد انتظار باید حداقل 5 باشند (یا حداقل 80% آنها بالای 5 و هیچکدام زیر 1 نباشد).</p>"
    },
    info_descriptive_frequencies: {
        isFinal: true,
        name: "آمار توصیفی: جداول فراوانی و درصد",
        description: "برای توصیف توزیع یک متغیر طبقه‌ای.",
        whenToUse: "زمانی که می‌خواهید فراوانی (تعداد) و درصد هر سطح از یک متغیر طبقه‌ای را نمایش دهید.",
        r_code: `
# data$categorical_var متغیر طبقه‌ای شما
# جدول فراوانی
frequency_table <- table(data$categorical_var)
print("جدول فراوانی:")
print(frequency_table)

# جدول درصد
percentage_table <- prop.table(frequency_table) * 100
print("جدول درصد:")
print(percentage_table)

# ترکیب در یک دیتافریم (با پکیج dplyr)
# library(dplyr)
# data %>%
#   count(categorical_var) %>%
#   mutate(percentage = n / sum(n) * 100)
        `,
        r_code_notes: "<p><code>table()</code> فراوانی‌ها را محاسبه می‌کند.</p><p><code>prop.table()</code> نسبت‌ها را محاسبه می‌کند که با ضرب در 100 به درصد تبدیل می‌شوند.</p><p>برای نمایش بهتر می‌توان از پکیج‌هایی مانند <code>dplyr</code> یا <code>gmodels</code> (برای تابع <code>CrossTable</code>) استفاده کرد.</p>"
    },
    test_chi_square_test_of_independence: {
        isFinal: true,
        name: "آزمون خی‌دو استقلال (Chi-squared Test of Independence)",
        description: "برای بررسی اینکه آیا ارتباطی بین دو متغیر طبقه‌ای وجود دارد یا خیر.",
        whenToUse: "زمانی که دو متغیر طبقه‌ای دارید و می‌خواهید بدانید آیا این دو متغیر از یکدیگر مستقل هستند یا با هم ارتباط (association) دارند. برای جداول contingency 2x2 یا بزرگتر.",
        r_code: `
# data$categorical_var1 اولین متغیر طبقه‌ای
# data$categorical_var2 دومین متغیر طبقه‌ای
contingency_table <- table(data$categorical_var1, data$categorical_var2)
print("جدول توافقی (Contingency Table):")
print(contingency_table)

test_result <- chisq.test(contingency_table)
print(test_result)

# برای جداول 2x2 با فراوانی‌های مورد انتظار کم (کمتر از 5)، آزمون دقیق فیشر ممکن است مناسب‌تر باشد:
# fisher.test(contingency_table)
        `,
        r_code_notes: "<p>ابتدا یک جدول توافقی (crosstabulation) از دو متغیر ایجاد می‌شود.</p><p>پیش‌فرض: تمام فراوانی‌های مورد انتظار باید حداقل 5 باشند (یا حداقل 80% آنها بالای 5 و هیچکدام زیر 1 نباشد). اگر این شرط برقرار نباشد، برای جداول 2x2 از آزمون دقیق فیشر (<code>fisher.test</code>) و برای جداول بزرگتر، شبیه‌سازی مقدار p در <code>chisq.test(..., simulate.p.value = TRUE)</code> را در نظر بگیرید.</p>"
    },
    test_mcnemar_test: {
        isFinal: true,
        name: "آزمون مک‌نمار (McNemar's Test)",
        description: "برای بررسی تغییر در نسبت‌ها برای داده‌های جفت شده (paired categorical data)، معمولاً برای متغیرهای وابسته دوتایی قبل و بعد از یک مداخله.",
        whenToUse: "زمانی که یک متغیر طبقه‌ای دوتایی (binary) دارید که دو بار روی یک گروه از افراد یا واحدهای جفت شده اندازه‌گیری شده است (مثلاً پاسخ مثبت/منفی قبل و بعد از درمان). این آزمون به تغییرات یا "عدم توافق" در پاسخ‌ها تمرکز دارد.",
        r_code: `
# فرض کنید داده‌ها به صورت یک جدول توافقی 2x2 از نتایج جفت شده هستند:
#        After: Yes   After: No
# Before: Yes   a        b
# Before: No    c        d
# یا داده‌ها در دو ستون مجزا برای قبل و بعد هستند: data$before_outcome, data$after_outcome

# اگر داده‌ها به صورت جدول هستند:
# my_table <- matrix(c(a, c, b, d), nrow = 2, byrow = TRUE,
#                    dimnames = list(Before = c("Yes", "No"), After = c("Yes", "No")))
# mcnemar.test(my_table)

# اگر داده‌ها در دو ستون هستند:
# data$before_outcome <- factor(data$before_outcome, levels = c("No", "Yes")) # یا 0, 1
# data$after_outcome <- factor(data$after_outcome, levels = c("No", "Yes")) # یا 0, 1
# mcnemar.test(x = data$before_outcome, y = data$after_outcome)
# یا
# mcnemar.test(table(data$before_outcome, data$after_outcome))

# مثال با داده‌های نمونه:
responses <- matrix(c(10, 2, 8, 20), nrow = 2, byrow = TRUE,
                    dimnames = list(Before = c("Success", "Failure"),
                                    After = c("Success", "Failure")))
print("جدول پاسخ‌های جفت شده:")
print(responses)
mcnemar.test(responses)
        `,
        r_code_notes: "<p>این آزمون بر روی سلول‌های خارج از قطر اصلی جدول توافقی (b و c) تمرکز دارد که نشان‌دهنده تغییر پاسخ‌ها هستند.</p><p>فقط برای داده‌های جفت شده و متغیرهای طبقه‌ای دوتایی مناسب است.</p><p>اگر b+c کوچک باشد (مثلاً کمتر از 25)، از نسخه دقیق (<code>mcnemar.exact()</code> از پکیج <code>exact2x2</code> یا <code>binom.test(b, n=b+c, p=0.5)</code>) استفاده می‌شود.</p>"
    },
    test_logistic_regression_simple: {
        isFinal: true,
        name: "رگرسیون لجستیک ساده (Simple Logistic Regression)",
        description: "برای بررسی رابطه بین یک متغیر وابسته دوتایی (binary) و یک متغیر مستقل پیوسته.",
        whenToUse: "زمانی که متغیر وابسته شما دو سطح دارد (مثلاً موفقیت/شکست، بله/خیر) و می‌خواهید تاثیر یک متغیر مستقل پیوسته را بر احتمال وقوع یکی از سطوح متغیر وابسته بررسی کنید.",
        r_code: `
# data$binary_dependent_var متغیر وابسته دوتایی (0 و 1، یا فاکتور با دو سطح)
# data$continuous_independent_var متغیر مستقل پیوسته
# اطمینان حاصل کنید که متغیر وابسته به درستی کدگذاری شده (معمولاً 0 برای عدم موفقیت، 1 برای موفقیت)
# data$binary_dependent_var <- factor(data$binary_dependent_var, levels = c("Level0", "Level1"))

model <- glm(binary_dependent_var ~ continuous_independent_var, 
             data = data, 
             family = binomial(link = "logit"))
summary(model)

# برای به دست آوردن نسبت شانس (Odds Ratios) و فواصل اطمینان:
# exp(coef(model))
# exp(confint(model))
        `,
        r_code_notes: "<p><code>family = binomial(link = \"logit\")</code> مشخص می‌کند که از مدل لجستیک استفاده شود.</p><p>خروجی شامل ضرایب (log-odds)، خطاهای استاندارد و p-values است. برای تفسیر آسان‌تر، ضرایب را به نسبت شانس (Odds Ratios) با استفاده از تابع <code>exp()</code> تبدیل کنید.</p>"
    },
    test_multiple_logistic_regression: {
        isFinal: true,
        name: "رگرسیون لجستیک چندگانه (Multiple Logistic Regression)",
        description: "برای بررسی رابطه بین یک متغیر وابسته دوتایی و دو یا چند متغیر مستقل (پیوسته یا طبقه‌ای).",
        whenToUse: "زمانی که متغیر وابسته شما دو سطح دارد و می‌خواهید تاثیر چندین متغیر مستقل را بر احتمال وقوع یکی از سطوح متغیر وابسته بررسی کنید.",
        r_code: `
# data$binary_dependent_var متغیر وابسته دوتایی (0/1 یا فاکتور)
# data$independent_var1, data$independent_var2, ... متغیرهای مستقل
# متغیرهای مستقل طبقه‌ای باید factor باشند.

model <- glm(binary_dependent_var ~ independent_var1 + independent_var2 + categorical_ind_var, 
             data = data, 
             family = binomial(link = "logit"))
summary(model)

# برای نسبت شانس (Odds Ratios):
# exp(coef(model))
# exp(confint(model))
        `,
        r_code_notes: "<p>مشابه رگرسیون لجستیک ساده، اما با چندین متغیر مستقل.</p><p>به هم‌خطی چندگانه بین متغیرهای مستقل توجه کنید.</p>"
    },
    test_logistic_regression_multi_cat_iv: {
        isFinal: true,
        name: "رگرسیون لجستیک (با متغیرهای مستقل طبقه‌ای)",
        description: "برای بررسی رابطه بین یک متغیر وابسته دوتایی و چندین متغیر مستقل که همگی طبقه‌ای هستند.",
        whenToUse: "زمانی که متغیر وابسته شما دو سطح دارد و تمام متغیرهای مستقل شما طبقه‌ای هستند و می‌خواهید تاثیر آن‌ها را بر احتمال وقوع یکی از سطوح متغیر وابسته بررسی کنید.",
        r_code: `
# data$binary_dependent_var متغیر وابسته دوتایی (0/1 یا فاکتور)
# data$cat_iv1, data$cat_iv2, ... متغیرهای مستقل طبقه‌ای (باید factor باشند)
# data$cat_iv1 <- factor(data$cat_iv1)
# data$cat_iv2 <- factor(data$cat_iv2)

model <- glm(binary_dependent_var ~ cat_iv1 + cat_iv2, 
             data = data, 
             family = binomial(link = "logit"))
summary(model)

# برای نسبت شانس (Odds Ratios):
# exp(coef(model))
# exp(confint(model))
        `,
        r_code_notes: "<p>این حالت خاصی از رگرسیون لجستیک چندگانه است که تمام پیش‌بین‌ها طبقه‌ای هستند.</p><p>R به طور خودکار برای متغیرهای فاکتور، متغیرهای ساختگی (dummy variables) ایجاد می‌کند. سطح مرجع (reference level) هر فاکتور مهم است.</p>"
    },
    test_multinomial_logistic_regression: {
        isFinal: true,
        name: "رگرسیون لجستیک چندجمله‌ای (Multinomial Logistic Regression)",
        description: "برای بررسی رابطه بین یک متغیر وابسته طبقه‌ای با بیش از دو سطح اسمی (nominal) و یک یا چند متغیر مستقل.",
        whenToUse: "زمانی که متغیر وابسته شما بیش از دو سطح دارد و این سطوح ترتیب خاصی ندارند (مثلاً انتخاب بین سه برند مختلف، سه گروه شغلی).",
        r_code: `
# نیاز به پکیج nnet یا VGAM
# install.packages("nnet")
library(nnet)

# data$multinomial_dv متغیر وابسته طبقه‌ای با k سطح (k > 2)، باید factor باشد.
# data$independent_var1, data$independent_var2, ... متغیرهای مستقل

# یک سطح از DV به عنوان مرجع انتخاب می‌شود (معمولاً اولین سطح به طور پیش‌فرض)
# data$multinomial_dv <- relevel(data$multinomial_dv, ref = "Level_Reference")

model <- multinom(multinomial_dv ~ independent_var1 + independent_var2, data = data)
summary(model)

# برای ضرایب و خطاهای استاندارد
# z_stats <- summary(model)$coefficients / summary(model)$standard.errors
# p_values <- (1 - pnorm(abs(z_stats), 0, 1)) * 2
# print(p_values)

# برای نسبت شانس نسبی (Relative Risk Ratios)
# exp(coef(model))
        `,
        r_code_notes: "<p>پکیج <code>nnet</code> (تابع <code>multinom</code>) یا <code>VGAM</code> (تابع <code>vglm</code> با خانواده <code>multinomial</code>) برای این مدل استفاده می‌شود.</p><p>نتایج به صورت log-odds (یا نسبت شانس نسبی پس از exponentiate کردن) برای هر سطح از متغیر وابسته نسبت به سطح مرجع ارائه می‌شوند.</p>"
    },
    test_ordinal_logistic_regression: {
        isFinal: true,
        name: "رگرسیون لجستیک ترتیبی (Ordinal Logistic Regression / Proportional Odds Model)",
        description: "برای بررسی رابطه بین یک متغیر وابسته طبقه‌ای ترتیبی (ordinal) و یک یا چند متغیر مستقل.",
        whenToUse: "زمانی که متغیر وابسته شما بیش از دو سطح دارد و این سطوح دارای ترتیب مشخصی هستند (مثلاً سطح رضایت: کم، متوسط، زیاد؛ یا مراحل یک بیماری).",
        r_code: `
# نیاز به پکیج MASS یا ordinal
# install.packages("MASS")
library(MASS) # برای تابع polr()

# data$ordinal_dv متغیر وابسته ترتیبی (باید ordered factor باشد)
# data$independent_var1, data$independent_var2, ... متغیرهای مستقل

# اطمینان از اینکه DV یک فاکتور مرتب شده است
# data$ordinal_dv <- factor(data$ordinal_dv, 
#                             levels = c("Low", "Medium", "High"), 
#                             ordered = TRUE)

model <- polr(ordinal_dv ~ independent_var1 + independent_var2, data = data, Hess = TRUE)
summary(model)

# برای ضرایب، خطاهای استاندارد و p-values
# ctable <- coef(summary(model))
# p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# ctable <- cbind(ctable, "p value" = p_values)
# print(ctable)

# برای نسبت شانس (Proportional Odds Ratios)
# exp(coef(model))
        `,
        r_code_notes: "<p>پکیج <code>MASS</code> (تابع <code>polr</code>) یا <code>ordinal</code> (تابع <code>clm</code>) رایج هستند.</p><p>این مدل فرض می‌کند که اثر متغیرهای مستقل بر شانس قرار گرفتن در یک سطح یا بالاتر، برای تمام نقاط برش (cut-points) یکسان است (فرض proportional odds). این فرض باید بررسی شود.</p><p>متغیر وابسته باید یک <code>ordered factor</code> باشد.</p>"
    },
     test_ordinal_logistic_regression_multi_iv: { // Similar to above, just emphasizing multi IV context
        isFinal: true,
        name: "رگرسیون لجستیک ترتیبی (با چندین متغیر مستقل)",
        description: "برای بررسی رابطه بین یک متغیر وابسته طبقه‌ای ترتیبی (ordinal) و دو یا چند متغیر مستقل (پیوسته یا طبقه‌ای).",
        whenToUse: "زمانی که متغیر وابسته شما دارای سطوح مرتب است و می‌خواهید تأثیر چندین پیش‌بین را بر آن بررسی کنید.",
        r_code: `
# نیاز به پکیج MASS یا ordinal
# install.packages("MASS")
library(MASS) # برای تابع polr()

# data$ordinal_dv متغیر وابسته ترتیبی (باید ordered factor باشد)
# data$independent_var1, data$independent_var2, data$cat_ind_var متغیرهای مستقل

# اطمینان از اینکه DV یک فاکتور مرتب شده است
# data$ordinal_dv <- factor(data$ordinal_dv, 
#                             levels = c("سطح1", "سطح2", "سطح3"), 
#                             ordered = TRUE)
# data$cat_ind_var <- factor(data$cat_ind_var)


model <- polr(ordinal_dv ~ independent_var1 + independent_var2 + cat_ind_var, 
              data = data, Hess = TRUE)
summary(model)

# برای ضرایب، خطاهای استاندارد و p-values
# (ctable <- coef(summary(model)))
# p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p_values))

# برای نسبت شانس (Proportional Odds Ratios)
# exp(coef(model))
        `,
        r_code_notes: "<p>این همان رگرسیون لجستیک ترتیبی است که در آن به طور مشخص ذکر شده که چندین متغیر مستقل (از انواع مختلف) می‌توانند وجود داشته باشند.</p><p>پیش‌فرض proportional odds همچنان پابرجاست و باید بررسی شود.</p>"
    },
    test_log_linear_model_or_mca: { // Broad category
        isFinal: true,
        name: "مدل‌های لگاریتمی-خطی (Log-linear Models) یا تحلیل تناظر چندگانه (MCA)",
        description: "مدل‌های لگاریتمی-خطی برای تحلیل روابط و همبستگی‌ها در جداول توافقی چندبعدی (بین چندین متغیر طبقه‌ای) استفاده می‌شوند. تحلیل تناظر چندگانه (MCA) یک روش کاهش بعد برای داده‌های طبقه‌ای است که ساختار روابط بین متغیرها را به صورت بصری نشان می‌دهد.",
        whenToUse: "زمانی که بیش از دو متغیر طبقه‌ای دارید و می‌خواهید الگوهای وابستگی (association) بین آن‌ها را بررسی کنید (Log-linear). یا زمانی که می‌خواهید ساختار و روابط بین چندین متغیر طبقه‌ای را با کاهش ابعاد و نمایش بصری بررسی کنید (MCA).",
        r_code: `
# --- مدل لگاریتمی-خطی (Log-linear Model) ---
# نیاز به پکیج MASS برای loglm یا استفاده از glm با خانواده poisson
# فرض کنید یک جدول توافقی سه بعدی دارید: table_3d <- table(data$cat_var1, data$cat_var2, data$cat_var3)

# مثال: مدل اشباع (saturated model) که تمام اثرات متقابل را شامل می‌شود
# library(MASS)
# sat_model <- loglm(~ cat_var1*cat_var2*cat_var3, data = my_dataframe_with_factors) 
# summary(sat_model)

# مثال: مدل استقلال کامل
# indep_model <- loglm(~ cat_var1 + cat_var2 + cat_var3, data = my_dataframe_with_factors)
# anova(indep_model, sat_model) # مقایسه مدل‌ها

# --- تحلیل تناظر چندگانه (Multiple Correspondence Analysis - MCA) ---
# نیاز به پکیج FactoMineR یا ca
# install.packages("FactoMineR")
library(FactoMineR)

# فرض کنید my_data_categorical یک دیتافریم فقط با متغیرهای طبقه‌ای (فاکتور) است
# mca_result <- MCA(my_data_categorical, graph = TRUE)
# summary(mca_result)
# plot(mca_result)
        `,
        r_code_notes: "<p><strong>مدل لگاریتمی-خطی:</strong> برای جداول توافقی با بیش از دو متغیر استفاده می‌شود تا ساختار وابستگی‌ها را مدل کند. <code>loglm</code> از پکیج <code>MASS</code> یا <code>glm</code> با <code>family = poisson</code> قابل استفاده است.</p><p><strong>تحلیل تناظر چندگانه (MCA):</strong> روشی شبیه PCA برای داده‌های طبقه‌ای. پکیج <code>FactoMineR</code> یا <code>ca</code> برای این کار مناسب هستند. این روش به خصوص برای داده‌های با تعداد زیادی متغیر طبقه‌ای مفید است.</p><p>انتخاب بین این دو بستگی به هدف تحقیق دارد: مدل‌سازی وابستگی‌ها یا کاهش بعد و بصری‌سازی.</p>"
    },


    // ## آزمون‌های مربوط به یک متغیر وابسته شمارشی (Count) ##
    test_poisson_goodness_of_fit: {
        isFinal: true,
        name: "آزمون نیکویی برازش برای توزیع پواسون",
        description: "برای بررسی اینکه آیا داده‌های شمارشی از توزیع پواسون با یک نرخ (λ) مشخص پیروی می‌کنند یا خیر.",
        whenToUse: "زمانی که یک متغیر شمارشی دارید و می‌خواهید بدانید آیا توزیع آن با توزیع پواسون (با یک پارامتر نرخ λ که ممکن است از داده‌ها برآورد شود یا از قبل مشخص باشد) مطابقت دارد.",
        r_code: `
# data$count_variable متغیر شمارشی شما
# نیاز به پکیج vcdExtra برای برخی آزمون‌های بهتر، یا محاسبات دستی

# روش ساده با استفاده از آزمون خی‌دو (اگر lambda از داده‌ها تخمین زده شود، درجه آزادی باید تنظیم شود)
observed_counts <- table(data$count_variable)
lambda_hat <- mean(data$count_variable) # تخمین lambda

# محاسبه احتمالات پواسون مورد انتظار
# max_observed_count <- max(as.numeric(names(observed_counts)))
# expected_probs <- dpois(0:max_observed_count, lambda = lambda_hat)
# expected_counts <- sum(observed_counts) * expected_probs

# این بخش نیاز به گروه‌بندی سلول‌های با فراوانی کم دارد و پیچیده است.

# یک روش ساده‌تر برای بررسی پراکندگی (dispersion):
# واریانس باید تقریباً برابر با میانگین باشد در توزیع پواسون
# print(paste("Mean:", mean(data$count_variable)))
# print(paste("Variance:", var(data$count_variable)))
# اگر واریانس خیلی بزرگتر از میانگین باشد، پراکندگی بیش از حد (overdispersion) وجود دارد.

# برای آزمون رسمی‌تر نیکویی برازش پواسون، از پکیج‌های تخصصی مانند 'vcd' یا 'countreg' استفاده کنید.
# library(vcd)
# gf <- goodfit(data$count_variable, type = "poisson", method = "MinChisq")
# summary(gf)
# plot(gf)
        `,
        r_code_notes: "<p>یک راه سریع برای بررسی اولیه، مقایسه میانگین و واریانس داده‌های شمارشی است. اگر واریانس به طور قابل توجهی از میانگین بزرگتر باشد (overdispersion) یا کوچکتر باشد (underdispersion)، توزیع پواسون ممکن است مناسب نباشد.</p><p>پکیج <code>vcd</code> (تابع <code>goodfit</code>) یا <code>countreg</code> ابزارهای بهتری برای آزمون نیکویی برازش توزیع‌های شمارشی ارائه می‌دهند.</p>"
    },
    info_descriptive_count: {
        isFinal: true,
        name: "آمار توصیفی برای داده‌های شمارشی",
        description: "توصیف توزیع یک متغیر شمارشی (مانند میانگین، واریانس، هیستوگرام).",
        whenToUse: "زمانی که می‌خواهید ویژگی‌های اصلی یک متغیر شمارشی را خلاصه و نمایش دهید.",
        r_code: `
# data$count_variable متغیر شمارشی شما
print(paste("Mean:", mean(data$count_variable)))
print(paste("Variance:", var(data$count_variable)))
print(paste("Median:", median(data$count_variable)))
print(paste("Min:", min(data$count_variable)))
print(paste("Max:", max(data$count_variable)))
print("Summary (Quantiles, etc.):")
print(summary(data$count_variable))

# هیستوگرام برای مشاهده توزیع
# hist(data$count_variable, main = "Histogram of Count Variable", xlab = "Count")

# جدول فراوانی
# print(table(data$count_variable))
        `,
        r_code_notes: "<p>توابع پایه‌ای R مانند <code>mean()</code>، <code>var()</code>، <code>summary()</code>، <code>hist()</code>، و <code>table()</code> برای این کار مفید هستند.</p><p>به شکل توزیع (مثلاً چولگی) و وجود مقادیر پرت توجه کنید.</p>"
    },
    test_poisson_regression: {
        isFinal: true,
        name: "رگرسیون پواسون (Poisson Regression)",
        description: "برای مدل‌سازی متغیرهای وابسته شمارشی و بررسی تاثیر متغیرهای مستقل بر نرخ شمارش.",
        whenToUse: "زمانی که متغیر وابسته شما شمارشی است (مثلاً تعداد وقوع یک رویداد) و می‌خواهید تاثیر متغیرهای مستقل (پیوسته یا طبقه‌ای) را بر آن بررسی کنید. پیش‌فرض اصلی این است که میانگین و واریانس داده‌های شمارشی (شرطی بر متغیرهای مستقل) برابر باشند (equidispersion).",
        r_code: `
# data$count_dependent_var متغیر وابسته شمارشی
# data$independent_var1, data$independent_var2, ... متغیرهای مستقل
model <- glm(count_dependent_var ~ independent_var1 + independent_var2, 
             data = data, 
             family = poisson(link = "log"))
summary(model)

# برای بررسی پراکندگی بیش از حد (overdispersion):
# E.g., Residual Deviance / Degrees of Freedom. If > 1.5-2, might be overdispersion.
# (dispersion_param <- summary(model)$deviance / summary(model)$df.residual)
# یا از آزمون‌های رسمی‌تر مانند پکیج 'AER' تابع 'dispersiontest'
# library(AER)
# dispersiontest(model)
        `,
        r_code_notes: "<p><code>family = poisson(link = \"log\")</code> مشخص می‌کند که از مدل پواسون با تابع پیوند لگاریتمی استفاده شود.</p><p>اگر پراکندگی بیش از حد (overdispersion) وجود داشته باشد (واریانس بزرگتر از میانگین)، مدل رگرسیون دوجمله‌ای منفی (Negative Binomial Regression) یا مدل‌های شبه‌پواسون (Quasi-Poisson) ممکن است مناسب‌تر باشند.</p>"
    },
    test_poisson_regression_offset: {
        isFinal: true,
        name: "رگرسیون پواسون با متغیر جبرانی (Offset)",
        description: "برای مدل‌سازی نرخ شمارش زمانی که دوره مشاهده یا میزان 'قرار گرفتن در معرض' (exposure) برای هر واحد متفاوت است.",
        whenToUse: "زمانی که متغیر وابسته شما شمارشی است و این شمارش‌ها در بازه‌های زمانی متفاوت یا با سطوح متفاوتی از 'فرصت' برای وقوع رویداد جمع‌آوری شده‌اند. متغیر جبرانی (offset) این تفاوت‌ها را در مدل لحاظ می‌کند.",
        r_code: `
# data$count_dependent_var متغیر وابسته شمارشی
# data$exposure_variable متغیر جبرانی (مثلاً زمان مشاهده، تعداد افراد در معرض)
# data$independent_var1, ... متغیرهای مستقل
# متغیر جبرانی باید در مقیاس لگاریتمی وارد شود
model <- glm(count_dependent_var ~ independent_var1 + independent_var2 + offset(log(exposure_variable)), 
             data = data, 
             family = poisson(link = "log"))
summary(model)
        `,
        r_code_notes: "<p>متغیر جبرانی (<code>exposure_variable</code>) با استفاده از تابع <code>offset()</code> و معمولاً پس از اعمال تابع لگاریتم (<code>log()</code>) به آن، در فرمول مدل وارد می‌شود.</p><p>این مدل در واقع <code>log(count/exposure)</code> را مدل می‌کند.</p><p>بررسی پراکندگی بیش از حد همچنان مهم است.</p>"
    },
    test_negative_binomial_regression: {
        isFinal: true,
        name: "رگرسیون دوجمله‌ای منفی (Negative Binomial Regression)",
        description: "برای مدل‌سازی متغیرهای وابسته شمارشی که پراکندگی بیش از حد (overdispersion) نشان می‌دهند (واریانس بزرگتر از میانگین است).",
        whenToUse: "زمانی که مدل رگرسیون پواسون به دلیل پراکندگی بیش از حد مناسب نیست. این مدل یک پارامتر پراکندگی اضافی برای تطبیق با واریانس بیشتر دارد.",
        r_code: `
# نیاز به پکیج MASS یا pscl یا glmmTMB
# install.packages("MASS")
library(MASS) # برای تابع glm.nb()

# data$count_dependent_var متغیر وابسته شمارشی
# data$independent_var1, ... متغیرهای مستقل
model <- glm.nb(count_dependent_var ~ independent_var1 + independent_var2, 
                data = data)
summary(model)

# برای مقایسه با مدل پواسون (مثلاً با آزمون نسبت درستنمایی)
# library(lmtest)
# poisson_model <- glm(count_dependent_var ~ independent_var1 + independent_var2, data = data, family = poisson)
# lrtest(poisson_model, model) # اگر معنادار باشد، مدل NB ترجیح داده می‌شود.
        `,
        r_code_notes: "<p>تابع <code>glm.nb()</code> از پکیج <code>MASS</code> یکی از راه‌های رایج برای اجرای این مدل است.</p><p>این مدل معمولاً زمانی استفاده می‌شود که آزمون پراکندگی برای مدل پواسون معنادار باشد یا نسبت واریانس به میانگین به طور قابل توجهی بزرگتر از 1 باشد.</p>"
    },
    test_negative_binomial_regression_offset: {
        isFinal: true,
        name: "رگرسیون دوجمله‌ای منفی با متغیر جبرانی (Offset)",
        description: "برای مدل‌سازی نرخ شمارش با پراکندگی بیش از حد و با لحاظ کردن دوره مشاهده یا میزان 'قرار گرفتن در معرض' (exposure) متفاوت.",
        whenToUse: "زمانی که هم پراکندگی بیش از حد دارید و هم نیاز به استفاده از متغیر جبرانی (offset) برای استانداردسازی نرخ‌ها.",
        r_code: `
# نیاز به پکیج MASS
library(MASS)

# data$count_dependent_var متغیر وابسته شمارشی
# data$exposure_variable متغیر جبرانی
# data$independent_var1, ... متغیرهای مستقل
model <- glm.nb(count_dependent_var ~ independent_var1 + independent_var2 + offset(log(exposure_variable)), 
                data = data)
summary(model)
        `,
        r_code_notes: "<p>ترکیبی از رگرسیون دوجمله‌ای منفی و استفاده از متغیر جبرانی.</p>"
    },
    test_zero_inflated_poisson_or_nb: {
        isFinal: true,
        name: "مدل‌های شمارشی با تورم صفر (Zero-Inflated Poisson/Negative Binomial)",
        description: "برای مدل‌سازی داده‌های شمارشی که تعداد زیادی صفر بیش از حد انتظار در توزیع پواسون یا دوجمله‌ای منفی دارند.",
        whenToUse: "زمانی که تعداد صفرها در متغیر وابسته شمارشی شما به طور قابل توجهی بیشتر از آن چیزی است که توسط مدل پواسون یا دوجمله‌ای منفی استاندارد پیش‌بینی می‌شود. این مدل‌ها فرض می‌کنند که صفرها از دو فرآیند ناشی می‌شوند: یک فرآیند که همیشه صفر تولید می‌کند (مثلاً عدم وجود شرایط لازم برای وقوع رویداد) و یک فرآیند شمارشی استاندارد (پواسون یا NB).",
        r_code: `
# نیاز به پکیج pscl یا glmmTMB
# install.packages("pscl")
library(pscl)

# data$count_dependent_var متغیر وابسته شمارشی
# data$independent_var_count متغیر(های) مستقل برای بخش شمارش
# data$independent_var_zero متغیر(های) مستقل برای بخش تورم صفر (می‌تواند همان قبلی‌ها باشد یا متفاوت)

# مدل پواسون با تورم صفر (ZIP)
# model_zip <- zeroinfl(count_dependent_var ~ independent_var_count | independent_var_zero, 
#                       data = data, 
#                       dist = "poisson")
# summary(model_zip)

# مدل دوجمله‌ای منفی با تورم صفر (ZINB)
# model_zinb <- zeroinfl(count_dependent_var ~ independent_var_count | independent_var_zero, 
#                        data = data, 
#                        dist = "negbin")
# summary(model_zinb)

# مثال ساده:
# فرض کنید var1 هم در بخش شمارش و هم در بخش تورم صفر اثر دارد
model_example_zinb <- zeroinfl(count_dependent_var ~ var1 | var1, 
                               data = data, dist = "negbin")
summary(model_example_zinb)
        `,
        r_code_notes: "<p>پکیج <code>pscl</code> (تابع <code>zeroinfl</code>) یا <code>glmmTMB</code> برای این مدل‌ها استفاده می‌شوند.</p><p>فرمول مدل دو بخش دارد که با <code>|</code> از هم جدا می‌شوند: بخش اول برای مدل شمارشی (پواسون یا NB) و بخش دوم برای مدل لجستیک که احتمال تولید صفر اضافی را مدل می‌کند.</p><p>انتخاب بین ZIP و ZINB بستگی به وجود پراکندگی بیش از حد در بخش شمارشی (غیر از تورم صفر) دارد.</p>"
    }
};

let currentNodeKey = 'start'; // شروع از گره 'start'

function displayNode(nodeKey) {
    const node = decisionTree[nodeKey];
    if (!node) {
        console.error("گره یافت نشد:", nodeKey);
        questionTextElement.textContent = "خطا: گره تعریف نشده است. لطفاً به توسعه‌دهنده اطلاع دهید.";
        optionsContainerElement.innerHTML = "";
        return;
    }

    currentNodeKey = nodeKey; // ذخیره گره فعلی

    if (node.isFinal) {
        // نمایش نتیجه نهایی (آزمون آماری)
        questionContainerElement.classList.add('hidden');
        resultContainerElement.classList.remove('hidden');
        resetButtonElement.classList.remove('hidden');

        testNameElement.textContent = node.name || "نام آزمون در دسترس نیست";
        testDescriptionElement.innerHTML = node.description ? node.description.replace(/\n/g, '<br>') : "توضیحات در دسترس نیست.";
        testWhenToUseElement.innerHTML = node.whenToUse ? node.whenToUse.replace(/\n/g, '<br>') : "اطلاعاتی در مورد زمان استفاده موجود نیست.";
        
        if (node.r_code) {
            rCodeElement.textContent = node.r_code.trim();
            // اگر کتابخانه هایلایت کد دارید، اینجا فراخوانی کنید
            // مثلاً: Prism.highlightAllUnder(resultContainerElement); // اگر پریزم برای کل بخش نتیجه است
            // یا Prism.highlightElement(rCodeElement);
        } else {
            rCodeElement.textContent = "کد R نمونه موجود نیست.";
        }
        
        if (node.r_code_notes) {
            rCodeNotesElement.innerHTML = node.r_code_notes; // از innerHTML برای رندر کردن تگ های p استفاده می کنیم
        } else {
            rCodeNotesElement.innerHTML = "";
        }

    } else {
        // نمایش سوال و گزینه‌ها
        questionContainerElement.classList.remove('hidden');
        resultContainerElement.classList.add('hidden');
        // دکمه ریست می‌تواند همیشه نمایش داده شود یا فقط در صفحه نتیجه
        // برای نمایش همیشگی (بعد از اولین سوال):
        // if (nodeKey !== 'start') {
        //     resetButtonElement.classList.remove('hidden');
        // } else {
        //     resetButtonElement.classList.add('hidden');
        // }
        // فعلا فقط در صفحه نتیجه نشان داده شود (طبق کد قبلی)
         resetButtonElement.classList.add('hidden');


        questionTextElement.textContent = node.question;
        optionsContainerElement.innerHTML = ""; // پاک کردن گزینه‌های قبلی

        node.options.forEach(option => {
            const button = document.createElement('button');
            button.textContent = option.text;
            button.onclick = () => displayNode(option.nextNode);
            // اضافه کردن کلاس برای استایل‌دهی در صورت نیاز
            // button.classList.add('option-button'); 
            optionsContainerElement.appendChild(button);
        });
    }
}

function resetGuide() {
    displayNode('start'); // بازگشت به سوال اول
    resultContainerElement.classList.add('hidden');
    questionContainerElement.classList.remove('hidden');
    resetButtonElement.classList.add('hidden');
}

function copyCode() {
    const codeToCopy = rCodeElement.innerText; 
    navigator.clipboard.writeText(codeToCopy).then(() => {
        alert('کد R در کلیپ‌بورد کپی شد!');
    }).catch(err => {
        console.error('خطا در کپی کردن کد: ', err);
        // ارائه یک راه حل جایگزین برای مرورگرهایی که clipboard API را پشتیبانی نمی‌کنند یا خطا می‌دهند
        try {
            const textArea = document.createElement('textarea');
            textArea.value = codeToCopy;
            document.body.appendChild(textArea);
            textArea.select();
            document.execCommand('copy');
            document.body.removeChild(textArea);
            alert('کد R در کلیپ‌بورد کپی شد (روش جایگزین)!');
        } catch (e) {
            alert('خطا در کپی کردن کد. لطفاً دستی کپی کنید.');
        }
    });
}

// Event Listeners
resetButtonElement.addEventListener('click', resetGuide);

// --- شروع برنامه ---
// نمایش اولین سوال هنگام بارگذاری صفحه
document.addEventListener('DOMContentLoaded', () => {
    if (typeof Prism !== 'undefined') {
        // اگر Prism.js را اضافه کردید، این خط را برای فعال سازی نگه دارید
        // Prism.highlightAll(); 
    }
    displayNode('start');
});
