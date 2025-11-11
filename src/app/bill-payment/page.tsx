'use client';

import React, { useState, useEffect } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { ProcessPaymentRequest, ProcessPaymentResponse, AccountBalance } from '@/types/account';
import { Input, Button } from '@/components/ui';

export default function BillPaymentPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const [step, setStep] = useState<'input' | 'confirm' | 'success'>('input');
  const [formData, setFormData] = useState<ProcessPaymentRequest>({
    accountId: searchParams.get('accountId') || '',
    cardNumber: '',
    confirmPayment: '',
  });
  const [accountBalance, setAccountBalance] = useState<AccountBalance | null>(null);
  const [paymentResponse, setPaymentResponse] = useState<ProcessPaymentResponse | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (formData.accountId) {
      fetchAccountBalance();
    }
  }, []);

  const fetchAccountBalance = async () => {
    if (!formData.accountId) return;
    
    try {
      const balance = await accountService.getAccountBalance(formData.accountId);
      setAccountBalance(balance);
      setError(null);
    } catch (err: any) {
      setError(err.message || 'Failed to fetch account balance');
      console.error(err);
    }
  };

  const handleAccountIdBlur = async () => {
    if (formData.accountId) {
      await fetchAccountBalance();
    }
  };

  const handleInitiatePayment = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!formData.accountId || !formData.cardNumber) {
      setError('Please enter both Account ID and Card Number');
      return;
    }
    
    if (formData.accountId.length > 11) {
      setError('Account ID must be 11 characters or less');
      return;
    }
    
    if (formData.cardNumber.length !== 16) {
      setError('Card number must be exactly 16 digits');
      return;
    }
    
    try {
      setLoading(true);
      setError(null);
      
      // First call without confirmation to get account info
      const response = await accountService.processPayment({
        accountId: formData.accountId,
        cardNumber: formData.cardNumber,
        confirmPayment: '',
      });
      
      setPaymentResponse(response);
      setStep('confirm');
    } catch (err: any) {
      setError(err.message || 'Failed to initiate payment');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleConfirmPayment = async () => {
    try {
      setLoading(true);
      setError(null);
      
      const response = await accountService.processPayment({
        accountId: formData.accountId,
        cardNumber: formData.cardNumber,
        confirmPayment: 'Y',
      });
      
      setPaymentResponse(response);
      setStep('success');
    } catch (err: any) {
      setError(err.message || 'Failed to process payment');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleCancelPayment = async () => {
    try {
      setLoading(true);
      
      await accountService.processPayment({
        accountId: formData.accountId,
        cardNumber: formData.cardNumber,
        confirmPayment: 'N',
      });
      
      setStep('input');
      setPaymentResponse(null);
      setError(null);
    } catch (err: any) {
      setError(err.message || 'Failed to cancel payment');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleReset = () => {
    setStep('input');
    setFormData({
      accountId: '',
      cardNumber: '',
      confirmPayment: '',
    });
    setAccountBalance(null);
    setPaymentResponse(null);
    setError(null);
  };

  if (step === 'success' && paymentResponse) {
    return (
      <div className="p-6 max-w-3xl mx-auto">
        <div className="bg-green-50 border border-green-200 rounded-lg p-6 mb-6">
          <div className="flex items-center mb-4">
            <div className="bg-green-500 text-white rounded-full w-12 h-12 flex items-center justify-center text-2xl mr-4">
              ✓
            </div>
            <div>
              <h1 className="text-2xl font-bold text-green-800">Payment Successful!</h1>
              <p className="text-green-700">{paymentResponse.message}</p>
            </div>
          </div>
        </div>

        <div className="bg-white shadow rounded-lg p-6 mb-6">
          <h2 className="text-xl font-semibold mb-4">Payment Details</h2>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700">Transaction ID</label>
              <p className="mt-1 text-gray-900 font-mono text-lg">{paymentResponse.transactionId}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700">Account ID</label>
              <p className="mt-1 text-gray-900">{paymentResponse.accountId}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700">Payment Amount</label>
              <p className="mt-1 text-gray-900 text-lg font-bold text-green-600">
                ${paymentResponse.paymentAmount?.toFixed(2)}
              </p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700">Previous Balance</label>
              <p className="mt-1 text-gray-900">${paymentResponse.previousBalance?.toFixed(2)}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700">New Balance</label>
              <p className="mt-1 text-gray-900 font-bold">${paymentResponse.newBalance?.toFixed(2)}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700">Timestamp</label>
              <p className="mt-1 text-gray-900">
                {paymentResponse.timestamp ? new Date(paymentResponse.timestamp).toLocaleString() : 'N/A'}
              </p>
            </div>
          </div>
        </div>

        <div className="bg-white shadow rounded-lg p-6 mb-6">
          <h2 className="text-xl font-semibold mb-4">Transaction Information</h2>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700">Transaction Type</label>
              <p className="mt-1 text-gray-900">{paymentResponse.transactionTypeCode}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700">Category Code</label>
              <p className="mt-1 text-gray-900">{paymentResponse.transactionCategoryCode}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700">Source</label>
              <p className="mt-1 text-gray-900">{paymentResponse.transactionSource}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700">Description</label>
              <p className="mt-1 text-gray-900">{paymentResponse.transactionDescription}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700">Merchant</label>
              <p className="mt-1 text-gray-900">{paymentResponse.merchantName}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700">Merchant ID</label>
              <p className="mt-1 text-gray-900">{paymentResponse.merchantId}</p>
            </div>
          </div>
        </div>

        <div className="flex gap-2">
          <Button onClick={() => router.push(`/transactions/${paymentResponse.transactionId}`)}>
            View Transaction
          </Button>
          <Button onClick={() => router.push(`/accounts/${paymentResponse.accountId}`)}>
            View Account
          </Button>
          <Button variant="secondary" onClick={handleReset}>
            Make Another Payment
          </Button>
        </div>
      </div>
    );
  }

  if (step === 'confirm' && paymentResponse) {
    return (
      <div className="p-6 max-w-3xl mx-auto">
        <h1 className="text-2xl font-bold mb-6">Confirm Bill Payment</h1>
        
        {error && (
          <div className="bg-red-50 border border-red-200 rounded p-4 mb-6">
            <p className="text-red-800">{error}</p>
          </div>
        )}

        <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-6 mb-6">
          <h2 className="text-lg font-semibold text-yellow-800 mb-2">
            Please Review Payment Details
          </h2>
          <p className="text-yellow-700">
            You are about to pay the full balance of this account. This action cannot be undone.
          </p>
        </div>

        <div className="bg-white shadow rounded-lg p-6 mb-6">
          <h2 className="text-xl font-semibold mb-4">Payment Summary</h2>
          <div className="space-y-4">
            <div className="flex justify-between items-center border-b pb-2">
              <span className="text-gray-700">Account ID:</span>
              <span className="font-semibold">{formData.accountId}</span>
            </div>
            
            <div className="flex justify-between items-center border-b pb-2">
              <span className="text-gray-700">Card Number:</span>
              <span className="font-mono">{formData.cardNumber}</span>
            </div>
            
            <div className="flex justify-between items-center border-b pb-2">
              <span className="text-gray-700">Current Balance:</span>
              <span className="font-semibold text-lg">
                ${accountBalance?.currentBalance.toFixed(2)}
              </span>
            </div>
            
            <div className="flex justify-between items-center pt-2">
              <span className="text-gray-700 text-lg font-semibold">Amount to Pay:</span>
              <span className="font-bold text-2xl text-green-600">
                ${accountBalance?.currentBalance.toFixed(2)}
              </span>
            </div>
          </div>
        </div>

        <div className="flex gap-2">
          <Button onClick={handleConfirmPayment} disabled={loading}>
            {loading ? 'Processing...' : 'Confirm Payment'}
          </Button>
          <Button variant="danger" onClick={handleCancelPayment} disabled={loading}>
            Cancel
          </Button>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-3xl mx-auto">
      <h1 className="text-2xl font-bold mb-6">Bill Payment</h1>
      
      <div className="bg-blue-50 border border-blue-200 rounded p-4 mb-6">
        <p className="text-sm text-blue-800">
          <strong>Business Rules:</strong> This payment will process the full current balance of the account. 
          You will be asked to confirm before the payment is processed.
        </p>
      </div>

      {error && (
        <div className="bg-red-50 border border-red-200 rounded p-4 mb-6">
          <p className="text-red-800">{error}</p>
        </div>
      )}
      
      <form onSubmit={handleInitiatePayment} className="space-y-4">
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-xl font-semibold mb-4">Payment Information</h2>
          
          <div className="space-y-4">
            <Input
              label="Account ID"
              value={formData.accountId}
              onChange={(e) => setFormData({ ...formData, accountId: e.target.value })}
              onBlur={handleAccountIdBlur}
              maxLength={11}
              required
              placeholder="Enter account ID (max 11 characters)"
            />
            
            {accountBalance && (
              <div className="bg-gray-50 border border-gray-200 rounded p-4">
                <div className="flex justify-between items-center">
                  <span className="text-gray-700">Current Balance:</span>
                  <span className="font-bold text-lg">
                    ${accountBalance.currentBalance.toFixed(2)}
                  </span>
                </div>
                <div className="flex justify-between items-center mt-2">
                  <span className="text-gray-700">Status:</span>
                  <span className={`px-2 py-1 rounded text-xs font-semibold ${
                    accountBalance.hasPositiveBalance 
                      ? 'bg-yellow-100 text-yellow-800' 
                      : 'bg-green-100 text-green-800'
                  }`}>
                    {accountBalance.hasPositiveBalance ? 'Balance Due' : 'Paid'}
                  </span>
                </div>
              </div>
            )}
            
            <Input
              label="Card Number"
              value={formData.cardNumber}
              onChange={(e) => {
                const value = e.target.value.replace(/\D/g, '');
                setFormData({ ...formData, cardNumber: value });
              }}
              maxLength={16}
              required
              placeholder="Enter 16-digit card number"
            />
          </div>
        </div>
        
        <div className="flex gap-2">
          <Button type="submit" disabled={loading || !accountBalance?.hasPositiveBalance}>
            {loading ? 'Processing...' : 'Continue to Confirmation'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/accounts')}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
