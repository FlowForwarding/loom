//
//  NCIEditServerView.m
//  Tapestry
//
//  Created by Infoblox Inc on 11/18/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import "NCIEditServerView.h"
//#import "NCIWebSocketConnector.h"

@interface NCIEditServerView()<UITextFieldDelegate, UITableViewDataSource,
        UITableViewDelegate, UIGestureRecognizerDelegate, UIWebViewDelegate> {
    UITextField *serverUrlEdit;
    UIButton *clearBtn;
    UIButton *goBtn;
    UITableView *bookmarksTable;
}

@property(nonatomic, strong) NSMutableArray *tapestryURLs;
@property(nonatomic, strong) NSString *currentUrl;

@end

static int bookmarkMaxRowsCount = 20;

static int editServerInputHeigth = 40;
static float iconDim = 40;

static float bookmarkRowHeigth = 44;
static int bookmarkVisibleRowsCount = 7;

static float leftIndent = 35;
static float topIndent = 25;
static float rightIndent = 120;

@implementation NCIEditServerView

- (id)initWithFrame:(CGRect)frame andView:(UIWebView *)webView{
    
    self = [self initWithFrame:frame];
    if (self) {
        self.mainView = webView;
        self.mainView.delegate = self;
        
        bookmarksTable = [[UITableView alloc] initWithFrame:CGRectMake(leftIndent,
                                                                       editServerInputHeigth,
                                                                       self.bounds.size.width - rightIndent - leftIndent,
                                                                       0)];
        self.tapestryURLs = [[NSUserDefaults standardUserDefaults] objectForKey:@"tapestryUrls"];
        if (!self.tapestryURLs){
            self.tapestryURLs = [[NSMutableArray alloc] init];
        }
        if (self.tapestryURLs.count == 0){
            self.currentUrl = nciDemoUrl;
        } else {
            self.currentUrl = self.tapestryURLs[0];
        }
        
        bookmarksTable.dataSource = self;
        bookmarksTable.delegate = self;
        [self addSubview:bookmarksTable];
        
        serverUrlEdit = [[UITextField alloc] initWithFrame:CGRectZero];
        serverUrlEdit.backgroundColor = [UIColor colorWithWhite:0.99 alpha:1];
        serverUrlEdit.autocorrectionType = UITextAutocorrectionTypeNo;
        serverUrlEdit.autocapitalizationType = UITextAutocapitalizationTypeNone;
        serverUrlEdit.layer.borderColor = [UIColor grayColor].CGColor;
        serverUrlEdit.layer.borderWidth = 0.2;
        serverUrlEdit.placeholder = nciSampleUrl;
        serverUrlEdit.text = [self getTapestryUrl];
        serverUrlEdit.layer.cornerRadius = 15;
        serverUrlEdit.contentVerticalAlignment = UIControlContentVerticalAlignmentCenter;
        serverUrlEdit.delegate = self;
        serverUrlEdit.returnKeyType = UIReturnKeyGo;
        [serverUrlEdit addTarget:self action:@selector(didChangeText) forControlEvents:UIControlEventEditingChanged];
        UILabel *serverEditLeftView = [[UILabel alloc] initWithFrame:CGRectMake(0, 0, 190, editServerInputHeigth)];
        serverEditLeftView.backgroundColor = [UIColor colorWithWhite:0.99 alpha:1];
        serverEditLeftView.text = NSLocalizedString(@"Tapestry Server:   ws://", nil);
        serverEditLeftView.font = [UIFont boldSystemFontOfSize:16];
        serverEditLeftView.textColor = [UIColor blackColor];
        serverUrlEdit.leftView = serverEditLeftView;
        serverUrlEdit.leftViewMode = UITextFieldViewModeAlways;
        [self addSubview:serverUrlEdit];
        
        clearBtn = [[UIButton alloc] initWithFrame:CGRectMake(0, 0, iconDim, iconDim)];
        [clearBtn setImage:[UIImage imageNamed:@"clear_input"] forState:UIControlStateNormal];
        [clearBtn addTarget:self action:@selector(clearInput) forControlEvents:UIControlEventTouchUpInside];
        clearBtn.hidden = YES;
        serverUrlEdit.rightView = clearBtn;
        serverUrlEdit.rightViewMode =  UITextFieldViewModeAlways;
        
        goBtn = [[UIButton alloc] initWithFrame:CGRectZero];
        [goBtn setImage:[UIImage imageNamed:@"go"] forState:UIControlStateNormal];
        [goBtn addTarget:self action:@selector(connectUrl) forControlEvents:UIControlEventTouchUpInside];
        goBtn.hidden = YES;
        [self addSubview:goBtn];
        
        self.backgroundColor = [UIColor clearColor];
        UITapGestureRecognizer *freeTap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(cancelUrlChanges)];
        self.userInteractionEnabled = YES;
        freeTap.numberOfTapsRequired = 1;
        freeTap.delegate = self;
        [self addGestureRecognizer:freeTap];
    }
    return self;
}

- (void)newTapestryUrl:(NSString *) newUrl{
    int ind;
    _currentUrl = newUrl;
    NSString *url;
    for (ind = 0; ind < self.tapestryURLs.count; ind++){
        url = self.tapestryURLs[ind];
        if ([url isEqualToString:newUrl]){
            [self.tapestryURLs removeObjectAtIndex:ind];
            break;
        }
    }
    [self.tapestryURLs insertObject:newUrl atIndex:0];
    if (self.tapestryURLs.count >  bookmarkMaxRowsCount){
        [self.tapestryURLs removeLastObject];
    }
    [[NSUserDefaults standardUserDefaults] setObject:self.tapestryURLs forKey:@"tapestryUrls"];
}

- (NSString *)getTapestryUrl{
    return _currentUrl;
}

- (void)removeURLAtIndex:(long)index{
    [_tapestryURLs removeObjectAtIndex:index];
    [[NSUserDefaults standardUserDefaults] setObject:self.tapestryURLs forKey:@"tapestryUrls"];
}


- (BOOL)gestureRecognizer:(UIGestureRecognizer *)gestureRecognizer shouldReceiveTouch:(UITouch *)touch{
    if ([touch.view isDescendantOfView:bookmarksTable]) {
        return NO;
    }
    return YES;
}


- (BOOL)textFieldShouldBeginEditing:(UITextField *)textField{
    self.frame = CGRectMake(self.frame.origin.x, self.frame.origin.y, self.bounds.size.width, self.superview.bounds.size.height);
    self.backgroundColor = [UIColor colorWithWhite:0.1 alpha:0.1];
    textField.backgroundColor = [UIColor whiteColor];
    [self didChangeText];
    goBtn.hidden = NO;
    
    long rowCount = self.tapestryURLs.count + 2;
    if (rowCount > bookmarkVisibleRowsCount){
        rowCount = bookmarkVisibleRowsCount;
    }
    bookmarksTable.frame = CGRectMake(leftIndent,
                                      editServerInputHeigth,
                                      self.bounds.size.width - rightIndent,
                                      rowCount *bookmarkRowHeigth);
    
    return YES;
}

- (void)textFieldDidEndEditing:(UITextField *)textField{
    textField.backgroundColor =  [UIColor colorWithWhite:0.99 alpha:1];
    clearBtn.hidden = YES;
}

- (BOOL)textFieldShouldReturn:(UITextField *)textField
{
    [self resignFirstResponder];
    [self connectUrl];
    return YES;
}

- (void)didChangeText{
    if (serverUrlEdit.text.length == 0){
        clearBtn.hidden = YES;
    } else {
        clearBtn.hidden = NO;
    }
}

- (void)clearInput{
    serverUrlEdit.text = @"";
    clearBtn.hidden = YES;
}

- (void)connectUrl{
    NSString *escapedUrl = [serverUrlEdit.text stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
    if (![serverUrlEdit.text isEqualToString:escapedUrl]){
        return;
    }
    goBtn.hidden = YES;
    [self newTapestryUrl:escapedUrl];
    [bookmarksTable reloadData];
    [self loadUrl:escapedUrl];
    [self resignFirstResponder];
}

- (void)loadUrl:(NSString *)loadUrl{
    NSString *reloadJs = [NSString stringWithFormat:@"NCI.connectionURL = 'ws://%@'; NCI.initSocket();", loadUrl];
    [self.mainView stringByEvaluatingJavaScriptFromString: reloadJs];
}

- (void)cancelUrlChanges{
    goBtn.hidden = YES;
    serverUrlEdit.text = [self getTapestryUrl];
    [self resignFirstResponder];
}

- (BOOL)resignFirstResponder{
    [serverUrlEdit resignFirstResponder];
    self.backgroundColor = [UIColor clearColor];
    self.frame = CGRectMake(self.frame.origin.x, self.frame.origin.y, self.bounds.size.width, editServerInputHeigth + topIndent);
    bookmarksTable.frame = CGRectMake(leftIndent,
                                      bookmarksTable.frame.origin.y,
                                      self.bounds.size.width - rightIndent, 0);
    return [super resignFirstResponder];
}

- (void)layoutSubviews {
    goBtn.frame = CGRectMake( self.bounds.size.width - 70, topIndent, iconDim, iconDim);
    serverUrlEdit.frame = CGRectMake(leftIndent, topIndent, self.bounds.size.width - rightIndent, editServerInputHeigth);
    bookmarksTable.frame = CGRectMake(leftIndent,
                                      editServerInputHeigth + topIndent + 2,
                                      self.bounds.size.width - rightIndent,
                                      bookmarksTable.frame.size.height);
}

#pragma table view data source

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section{
    return self.tapestryURLs.count + 2;
}

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath{
    return bookmarkRowHeigth;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath{
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"BookMarkCell"];
    if (cell == nil) {
        cell = [[UITableViewCell alloc] initWithStyle:UITableViewCellStyleDefault reuseIdentifier:@"BookMarkCell"];
        cell.selectionStyle = UITableViewCellSelectionStyleGray;
        cell.textLabel.font = [UIFont systemFontOfSize:14];
        UIButton *accessory = [[UIButton alloc] initWithFrame:CGRectMake(0, 0, 40, 40)];
        [accessory setImage:[UIImage imageNamed:@"delete_icon"] forState:UIControlStateNormal];
        [accessory addTarget:self action:@selector(checkButtonTapped:event:) forControlEvents:UIControlEventTouchUpInside];
        cell.accessoryView = accessory;
        [cell setAccessoryType:UITableViewCellAccessoryDetailDisclosureButton];
    }
    [cell.accessoryView setHidden:(indexPath.row < 2)];
    if (indexPath.row == 0){
        cell.textLabel.text = nciDemoUrl;
    } else  if (indexPath.row == 1){
        cell.textLabel.text = nciSampleUrl;
    } else {
        cell.textLabel.text = self.tapestryURLs[indexPath.row - 2];
    }

    return cell;
}

- (void)checkButtonTapped:(id)sender event:(id)event{
    NSSet *touches = [event allTouches];
    UITouch *touch = [touches anyObject];
    CGPoint currentTouchPosition = [touch locationInView:bookmarksTable];
    NSIndexPath *indexPath = [bookmarksTable indexPathForRowAtPoint: currentTouchPosition];
    if (indexPath != nil){
        [self tableView: bookmarksTable accessoryButtonTappedForRowWithIndexPath: indexPath];
    }
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath{
    if (indexPath.row  > 1){
        serverUrlEdit.text = self.tapestryURLs[indexPath.row - 2];
        [self connectUrl];
    } else if (indexPath.row  == 1){
        serverUrlEdit.text = nciSampleUrl;
    } else {
        serverUrlEdit.text = nciDemoUrl;
        [self connectUrl];
    }
}

- (void)tableView:(UITableView *)tableView accessoryButtonTappedForRowWithIndexPath:(NSIndexPath *)indexPath{
    if (indexPath.row > 1){
        [self removeURLAtIndex:indexPath.row - 2];
        [tableView deleteRowsAtIndexPaths:@[indexPath] withRowAnimation:UITableViewRowAnimationTop];
    }
}

- (void)webViewDidFinishLoad:(UIWebView *)webView{
    [self loadUrl:[self getTapestryUrl]];
}

@end
