using Calcium.SourceGeneration;
using Calcium.UIModel;

namespace Example
{
	public partial class MainWindowModel : ViewModelBase
	{
		string bar;

		public string Bar
		{
			get => bar;
			set => Set(ref bar, value);
		}

		[NotifyPropertyChanged]
		string customer1 = "Jane";

		[NotifyPropertyChanged]
		string customer2 = "Siddharth";

	}
}
