'use client';

import React, { useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { billPaymentService } from '@/services/billPaymentService';
import { AccountBalance, ProcessPaymentResponse } from '@/types/bill-payment';
import { Input, Button } from '@/components/ui';

export default function BillPaymentPage() {
  const router = useRouter();
  const [accountId, setAccountId] = useState('');
  const [cardNumber, setCardNumber] = useState('');
  const [accountBalance, setAccountBalance] = useState<AccountBalance | null>(null);
  const [confirmationRequired, setConfirmationRequired] = useState(false);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState<ProcessPaymentResponse | null>(null);
  const [currentDate] = useState(new Date().toLocaleDateString());
  const [currentTime] = useState(new Date().toLocaleTimeString());

  const handleAccountLookup = useCallback(async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!accountId.trim()) {
      setError('Acct ID can NOT be empty...');
      return;
    }

    if (!cardNumber.trim()) {
      setError('Card Number can NOT be empty...');
      return;
    }

    try {
      setLoading(true);
      setError(null);
      setSuccess(null);

      const balance = await billPaymentService.getAccountBalance(accountId);
      
      if (!balance.hasPositiveBalance || balance.currentBalance <= 0) {
        setError('You have nothing to pay...');
        setAccountBalance(null);
        setConfirmationRequired(false);
        return;
      }

      setAccountBalance(balance);
      setConfirmationRequired(true);
    } catch (err: any) {
      setError(err.message || 'Unable to lookup Account...');
      setAccountBalance(null);
      setConfirmationRequired(false);
    } finally {
      setLoading(false);
    }
  }, [accountId, cardNumber]);

  const handleConfirmPayment = useCallback(async () => {
    try {
      setLoading(true);
      setError(null);

      const response = await billPaymentService.processPayment({
        accountId,
        cardNumber,
        confirmPayment: 'Y',
      });

      setSuccess(response);
      setAccountBalance(null);
      setConfirmationRequired(false);
      setAccountId('');
      setCardNumber('');
    } catch (err: any) {
      setError(err.message || 'Unable to Add Bill pay Transaction...');
    } finally {
      setLoading(false);
    }
  }, [accountId, cardNumber]);

  const handleCancelPayment = useCallback(() => {
    setAccountBalance(null);
    setConfirmationRequired(false);
    setError(null);
    setSuccess(null);
  }, []);

  const handleClearScreen = useCallback(() => {
    setAccountId('');
    setCardNumber('');
    setAccountBalance(null);
    setConfirmationRequired(false);
    setError(null);
    setSuccess(null);
  }, []);

  const handleBack = useCallback(() => {
    router.back();
  }, [router]);

  return (
    <div className="p-6 max-w-4xl mx-auto">
      {/* Header Section */}
      <div className="mb-6">
        <div className="flex justify-between items-center mb-4">
          <h1 className="text-3xl font-bold">Bill Payment</h1>
          <div className="text-sm text-gray-600">
            <div>{currentDate}</div>
            <div>{currentTime}</div>
          </div>
        </div>
        <p className="text-gray-600">
          View your account balance and process full payment
        </p>
      </div>

      {/* Error Message */}
      {error && (
        <div className="mb-6 p-4 bg-red-50 border border-red-200 rounded-lg">
          <p className="text-red-800 font-semibold">{error}</p>
        </div>
      )}

      {/* Success Message */}
      {success && success.transactionId && (
        <div className="mb-6 p-4 bg-green-50 border border-green-200 rounded-lg">
          <p className="text-green-800 font-semibold">
            Payment successful. Your Transaction ID is {success.transactionId}.
          </p>
          <div className="mt-4 space-y-2 text-sm text-green-700">
            <div className="flex justify-between">
              <span className="font-semibold">Account ID:</span>
              <span>{success.accountId}</span>
            </div>
            <div className="flex justify-between">
              <span className="font-semibold">Payment Amount:</span>
              <span>${success.paymentAmount?.toFixed(2)}</span>
            </div>
            <div className="flex justify-between">
              <span className="font-semibold">Previous Balance:</span>
              <span>${success.previousBalance?.toFixed(2)}</span>
            </div>
            <div className="flex justify-between">
              <span className="font-semibold">New Balance:</span>
              <span>${success.newBalance?.toFixed(2)}</span>
            </div>
            <div className="flex justify-between">
              <span className="font-semibold">Transaction Time:</span>
              <span>{success.timestamp ? new Date(success.timestamp).toLocaleString() : ''}</span>
            </div>
          </div>
        </div>
      )}

      {/* Account Information Form */}
      {!confirmationRequired && !success && (
        <form onSubmit={handleAccountLookup} className="bg-white shadow rounded-lg p-6 space-y-4">
          <h2 className="text-xl font-semibold mb-4">Account Information</h2>
          
          <Input
            label="Account ID"
            value={accountId}
            onChange={(e) => setAccountId(e.target.value)}
            placeholder="Enter Account ID (11 characters)"
            maxLength={11}
            required
            disabled={loading}
          />
          
          <Input
            label="Card Number"
            value={cardNumber}
            onChange={(e) => setCardNumber(e.target.value)}
            placeholder="Enter Card Number (16 digits)"
            maxLength={16}
            required
            disabled={loading}
          />
          
          <div className="flex gap-2 pt-4">
            <Button type="submit" disabled={loading}>
              {loading ? 'Processing...' : 'Check Balance'}
            </Button>
            <Button
              type="button"
              variant="secondary"
              onClick={handleClearScreen}
              disabled={loading}
            >
              Clear
            </Button>
            <Button
              type="button"
              variant="secondary"
              onClick={handleBack}
              disabled={loading}
            >
              Back
            </Button>
          </div>
        </form>
      )}

      {/* Payment Confirmation Section */}
      {confirmationRequired && accountBalance && (
        <div className="bg-white shadow rounded-lg p-6 space-y-6">
          <h2 className="text-xl font-semibold">Payment Confirmation</h2>
          
          <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
            <p className="text-blue-800 font-semibold mb-4">
              Confirm to make a bill payment...
            </p>
            <div className="space-y-2 text-sm">
              <div className="flex justify-between">
                <span className="font-semibold text-gray-700">Account ID:</span>
                <span className="text-gray-900">{accountBalance.accountId}</span>
              </div>
              <div className="flex justify-between">
                <span className="font-semibold text-gray-700">Current Balance:</span>
                <span className="text-gray-900 text-lg font-bold">
                  ${accountBalance.currentBalance.toFixed(2)}
                </span>
              </div>
              <div className="flex justify-between">
                <span className="font-semibold text-gray-700">Payment Amount:</span>
                <span className="text-gray-900 text-lg font-bold">
                  ${accountBalance.currentBalance.toFixed(2)}
                </span>
              </div>
              <div className="flex justify-between">
                <span className="font-semibold text-gray-700">New Balance After Payment:</span>
                <span className="text-gray-900 text-lg font-bold">$0.00</span>
              </div>
            </div>
          </div>

          <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4">
            <p className="text-yellow-800 text-sm">
              <strong>Note:</strong> This will process a full balance payment. 
              Your entire current balance will be paid and your account balance will be set to $0.00.
            </p>
          </div>

          <div className="flex gap-2 pt-4">
            <Button onClick={handleConfirmPayment} disabled={loading}>
              {loading ? 'Processing Payment...' : 'Confirm Payment (Y)'}
            </Button>
            <Button
              variant="danger"
              onClick={handleCancelPayment}
              disabled={loading}
            >
              Cancel Payment (N)
            </Button>
          </div>
        </div>
      )}

      {/* Instructions */}
      {!confirmationRequired && !success && (
        <div className="mt-6 bg-gray-50 border border-gray-200 rounded-lg p-4">
          <h3 className="font-semibold text-gray-700 mb-2">Instructions:</h3>
          <ul className="text-sm text-gray-600 space-y-1 list-disc list-inside">
            <li>Enter your Account ID (11 characters) and Card Number (16 digits)</li>
            <li>Click "Check Balance" to view your current account balance</li>
            <li>Review the payment details and confirm to process full payment</li>
            <li>Your account balance will be set to $0.00 after successful payment</li>
            <li>A unique Transaction ID will be generated for your records</li>
          </ul>
        </div>
      )}
    </div>
  );
}
