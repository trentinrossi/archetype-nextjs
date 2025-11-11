'use client';

import React, { useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { billPaymentService } from '@/services/billPaymentService';
import { AccountBalance, ProcessPaymentResponse, Transaction } from '@/types/bill-payment';
import { Input, Button, Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui';

type PaymentStep = 'input' | 'confirm' | 'success';

export default function BillPaymentPage() {
  const router = useRouter();
  
  // Form state
  const [accountId, setAccountId] = useState('');
  const [cardNumber, setCardNumber] = useState('');
  
  // UI state
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  const [currentStep, setCurrentStep] = useState<PaymentStep>('input');
  
  // Data state
  const [accountBalance, setAccountBalance] = useState<AccountBalance | null>(null);
  const [paymentResponse, setPaymentResponse] = useState<ProcessPaymentResponse | null>(null);
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [showTransactions, setShowTransactions] = useState(false);

  // Validation
  const validateAccountId = (value: string): string | null => {
    if (!value || value.trim() === '') {
      return 'Acct ID can NOT be empty...';
    }
    if (value.length > 11) {
      return 'Account ID must be 11 characters or less';
    }
    return null;
  };

  const validateCardNumber = (value: string): string | null => {
    if (!value || value.trim() === '') {
      return 'Card Number can NOT be empty...';
    }
    if (value.length > 16) {
      return 'Card Number must be 16 characters or less';
    }
    return null;
  };

  // Fetch account balance and show confirmation
  const handleCheckBalance = async (e: React.FormEvent) => {
    e.preventDefault();
    
    // Validate inputs
    const accountIdError = validateAccountId(accountId);
    if (accountIdError) {
      setError(accountIdError);
      return;
    }
    
    const cardNumberError = validateCardNumber(cardNumber);
    if (cardNumberError) {
      setError(cardNumberError);
      return;
    }

    try {
      setLoading(true);
      setError(null);
      setSuccessMessage(null);

      // Get account balance
      const balance = await billPaymentService.getAccountBalance(accountId);
      setAccountBalance(balance);

      // Check if there's a balance to pay
      if (!balance.hasPositiveBalance || balance.currentBalance <= 0) {
        setError('You have nothing to pay...');
        setLoading(false);
        return;
      }

      // Move to confirmation step
      setCurrentStep('confirm');
    } catch (err: any) {
      setError(err.message || 'Unable to lookup Account...');
      console.error('Error fetching account balance:', err);
    } finally {
      setLoading(false);
    }
  };

  // Process payment with confirmation
  const handleConfirmPayment = async (confirm: boolean) => {
    if (!confirm) {
      // User cancelled - reset to input step
      handleClearScreen();
      return;
    }

    try {
      setLoading(true);
      setError(null);

      // Process payment with confirmation
      const response = await billPaymentService.processPayment({
        accountId,
        cardNumber,
        confirmPayment: 'Y',
      });

      setPaymentResponse(response);
      
      if (response.transactionId) {
        setSuccessMessage(`Payment successful. Your Transaction ID is ${response.transactionId}.`);
        setCurrentStep('success');
      } else {
        setError(response.message || 'Payment processing failed');
      }
    } catch (err: any) {
      setError(err.message || 'Unable to Add Bill pay Transaction...');
      console.error('Error processing payment:', err);
    } finally {
      setLoading(false);
    }
  };

  // Clear screen and reset to initial state
  const handleClearScreen = () => {
    setAccountId('');
    setCardNumber('');
    setError(null);
    setSuccessMessage(null);
    setAccountBalance(null);
    setPaymentResponse(null);
    setCurrentStep('input');
    setShowTransactions(false);
    setTransactions([]);
  };

  // Fetch transaction history
  const fetchTransactions = useCallback(async () => {
    if (!accountId) return;

    try {
      setLoading(true);
      const txns = await billPaymentService.getBillPaymentTransactions(accountId);
      setTransactions(txns);
      setShowTransactions(true);
    } catch (err: any) {
      console.error('Error fetching transactions:', err);
      setError('Failed to load transaction history');
    } finally {
      setLoading(false);
    }
  }, [accountId]);

  // Format currency
  const formatCurrency = (amount: number): string => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  // Format date
  const formatDate = (dateString: string): string => {
    return new Date(dateString).toLocaleString('en-US', {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
    });
  };

  return (
    <div className="p-6 max-w-4xl mx-auto">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Bill Payment</h1>
        <div className="flex gap-2">
          <Button variant="secondary" onClick={() => router.push('/')}>
            Return to Main Menu
          </Button>
        </div>
      </div>

      {/* Error Message */}
      {error && (
        <div className="mb-4 p-4 bg-red-50 border border-red-200 rounded-lg">
          <p className="text-red-800 font-semibold">{error}</p>
        </div>
      )}

      {/* Success Message */}
      {successMessage && (
        <div className="mb-4 p-4 bg-green-50 border border-green-200 rounded-lg">
          <p className="text-green-800 font-semibold">{successMessage}</p>
        </div>
      )}

      {/* Input Step */}
      {currentStep === 'input' && (
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-xl font-semibold mb-4">Account Information</h2>
          
          <form onSubmit={handleCheckBalance} className="space-y-4">
            <Input
              label="Account ID"
              value={accountId}
              onChange={(e) => {
                setAccountId(e.target.value);
                setError(null);
              }}
              placeholder="Enter Account ID (max 11 characters)"
              maxLength={11}
              required
              disabled={loading}
            />
            
            <Input
              label="Card Number"
              value={cardNumber}
              onChange={(e) => {
                setCardNumber(e.target.value);
                setError(null);
              }}
              placeholder="Enter Card Number (max 16 characters)"
              maxLength={16}
              required
              disabled={loading}
            />
            
            <div className="flex gap-2 pt-4">
              <Button type="submit" disabled={loading}>
                {loading ? 'Checking...' : 'Check Balance'}
              </Button>
              <Button
                type="button"
                variant="secondary"
                onClick={handleClearScreen}
                disabled={loading}
              >
                Clear
              </Button>
            </div>
          </form>
        </div>
      )}

      {/* Confirmation Step */}
      {currentStep === 'confirm' && accountBalance && (
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-xl font-semibold mb-4">Payment Confirmation</h2>
          
          <div className="space-y-4 mb-6">
            <div className="grid grid-cols-2 gap-4">
              <div>
                <label className="block text-sm font-semibold text-gray-700">Account ID</label>
                <p className="mt-1 text-gray-900">{accountBalance.accountId}</p>
              </div>
              
              <div>
                <label className="block text-sm font-semibold text-gray-700">Current Balance</label>
                <p className="mt-1 text-gray-900 text-lg font-bold text-red-600">
                  {formatCurrency(accountBalance.currentBalance)}
                </p>
              </div>
            </div>

            <div className="p-4 bg-blue-50 border border-blue-200 rounded-lg">
              <p className="text-blue-800">
                Confirm to make a bill payment...
              </p>
              <p className="text-blue-900 font-semibold mt-2">
                Payment Amount: {formatCurrency(accountBalance.currentBalance)}
              </p>
            </div>
          </div>
          
          <div className="flex gap-2">
            <Button 
              onClick={() => handleConfirmPayment(true)}
              disabled={loading}
            >
              {loading ? 'Processing...' : 'Confirm Payment (Y)'}
            </Button>
            <Button
              variant="danger"
              onClick={() => handleConfirmPayment(false)}
              disabled={loading}
            >
              Cancel Payment (N)
            </Button>
          </div>
        </div>
      )}

      {/* Success Step */}
      {currentStep === 'success' && paymentResponse && (
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-xl font-semibold mb-4 text-green-700">Payment Successful</h2>
          
          <div className="space-y-4 mb-6">
            <div className="grid grid-cols-2 gap-4">
              <div>
                <label className="block text-sm font-semibold text-gray-700">Transaction ID</label>
                <p className="mt-1 text-gray-900 font-mono text-lg font-bold">
                  {paymentResponse.transactionId}
                </p>
              </div>
              
              <div>
                <label className="block text-sm font-semibold text-gray-700">Account ID</label>
                <p className="mt-1 text-gray-900">{paymentResponse.accountId}</p>
              </div>
              
              <div>
                <label className="block text-sm font-semibold text-gray-700">Payment Amount</label>
                <p className="mt-1 text-gray-900 font-bold text-green-600">
                  {formatCurrency(paymentResponse.paymentAmount || 0)}
                </p>
              </div>
              
              <div>
                <label className="block text-sm font-semibold text-gray-700">New Balance</label>
                <p className="mt-1 text-gray-900 font-bold">
                  {formatCurrency(paymentResponse.newBalance || 0)}
                </p>
              </div>
              
              {paymentResponse.timestamp && (
                <div className="col-span-2">
                  <label className="block text-sm font-semibold text-gray-700">Transaction Date/Time</label>
                  <p className="mt-1 text-gray-900">{formatDate(paymentResponse.timestamp)}</p>
                </div>
              )}
            </div>

            {paymentResponse.transactionDescription && (
              <div>
                <label className="block text-sm font-semibold text-gray-700">Description</label>
                <p className="mt-1 text-gray-900">{paymentResponse.transactionDescription}</p>
              </div>
            )}

            {paymentResponse.merchantName && (
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <label className="block text-sm font-semibold text-gray-700">Merchant</label>
                  <p className="mt-1 text-gray-900">{paymentResponse.merchantName}</p>
                </div>
                
                {paymentResponse.merchantCity && paymentResponse.merchantCity !== 'N/A' && (
                  <div>
                    <label className="block text-sm font-semibold text-gray-700">Location</label>
                    <p className="mt-1 text-gray-900">
                      {paymentResponse.merchantCity}
                      {paymentResponse.merchantZip && paymentResponse.merchantZip !== 'N/A' && 
                        `, ${paymentResponse.merchantZip}`
                      }
                    </p>
                  </div>
                )}
              </div>
            )}
          </div>
          
          <div className="flex gap-2">
            <Button onClick={handleClearScreen}>
              Make Another Payment
            </Button>
            <Button
              variant="secondary"
              onClick={fetchTransactions}
              disabled={loading}
            >
              {loading ? 'Loading...' : 'View Transaction History'}
            </Button>
            <Button
              variant="secondary"
              onClick={() => router.push('/')}
            >
              Return to Main Menu
            </Button>
          </div>
        </div>
      )}

      {/* Transaction History */}
      {showTransactions && transactions.length > 0 && (
        <div className="mt-6 bg-white shadow rounded-lg p-6">
          <div className="flex justify-between items-center mb-4">
            <h2 className="text-xl font-semibold">Bill Payment Transaction History</h2>
            <Button
              size="sm"
              variant="secondary"
              onClick={() => setShowTransactions(false)}
            >
              Hide
            </Button>
          </div>
          
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>Transaction ID</TableHead>
                <TableHead>Date/Time</TableHead>
                <TableHead>Description</TableHead>
                <TableHead>Amount</TableHead>
                <TableHead>Card Number</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {transactions.map((txn) => (
                <TableRow key={txn.transactionId}>
                  <TableCell>
                    <span className="font-mono">{txn.transactionId}</span>
                  </TableCell>
                  <TableCell>
                    {formatDate(txn.processingTimestamp)}
                  </TableCell>
                  <TableCell>
                    {txn.description}
                  </TableCell>
                  <TableCell>
                    <span className="font-semibold text-green-600">
                      {formatCurrency(txn.amount)}
                    </span>
                  </TableCell>
                  <TableCell>
                    <span className="font-mono">
                      {txn.cardNumber.replace(/(\d{4})(\d{4})(\d{4})(\d{4})/, '$1 $2 $3 $4')}
                    </span>
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </div>
      )}

      {showTransactions && transactions.length === 0 && (
        <div className="mt-6 bg-white shadow rounded-lg p-6">
          <p className="text-center text-gray-500">No bill payment transactions found for this account.</p>
        </div>
      )}
    </div>
  );
}
