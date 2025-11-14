'use client';

import React, { useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { billPaymentService } from '@/services/billPaymentService';
import { BillPaymentState } from '@/types/billPayment';
import { Input, Button } from '@/components/ui';

export default function BillPaymentPage() {
  const router = useRouter();
  const [state, setState] = useState<BillPaymentState>({
    accountId: '',
    currentBalance: null,
    confirmation: '',
    isAccountValid: false,
    hasBalance: false,
    isProcessing: false,
    errorMessage: null,
    successMessage: null,
    transactionId: null,
  });

  const [accountIdTouched, setAccountIdTouched] = useState(false);
  const [confirmationTouched, setConfirmationTouched] = useState(false);

  const clearMessages = useCallback(() => {
    setState((prev) => ({
      ...prev,
      errorMessage: null,
      successMessage: null,
    }));
  }, []);

  const handleAccountIdChange = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    const value = e.target.value;
    setState((prev) => ({
      ...prev,
      accountId: value,
      currentBalance: null,
      isAccountValid: false,
      hasBalance: false,
      errorMessage: null,
      successMessage: null,
    }));
  }, []);

  const handleAccountIdBlur = useCallback(async () => {
    setAccountIdTouched(true);
    clearMessages();

    const trimmedAccountId = state.accountId.trim();

    if (!trimmedAccountId) {
      setState((prev) => ({
        ...prev,
        errorMessage: 'Acct ID can NOT be empty...',
        isAccountValid: false,
        hasBalance: false,
        currentBalance: null,
      }));
      return;
    }

    try {
      setState((prev) => ({ ...prev, isProcessing: true }));

      const validationResult = await billPaymentService.validateAccount({
        acctId: trimmedAccountId,
      });

      if (!validationResult.valid) {
        setState((prev) => ({
          ...prev,
          errorMessage: validationResult.message,
          isAccountValid: false,
          hasBalance: false,
          currentBalance: null,
          isProcessing: false,
        }));
        return;
      }

      const balanceResult = await billPaymentService.checkBalance({
        acctId: trimmedAccountId,
      });

      if (!balanceResult.hasBalance) {
        setState((prev) => ({
          ...prev,
          errorMessage: balanceResult.message,
          isAccountValid: true,
          hasBalance: false,
          currentBalance: balanceResult.currentBalance,
          isProcessing: false,
        }));
        return;
      }

      setState((prev) => ({
        ...prev,
        isAccountValid: true,
        hasBalance: true,
        currentBalance: balanceResult.currentBalance,
        errorMessage: null,
        isProcessing: false,
      }));
    } catch (error: any) {
      setState((prev) => ({
        ...prev,
        errorMessage: error.message || 'Failed to validate account',
        isAccountValid: false,
        hasBalance: false,
        currentBalance: null,
        isProcessing: false,
      }));
    }
  }, [state.accountId, clearMessages]);

  const handleConfirmationChange = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    const value = e.target.value.toUpperCase();
    setState((prev) => ({
      ...prev,
      confirmation: value,
      errorMessage: null,
      successMessage: null,
    }));
  }, []);

  const handleConfirmationBlur = useCallback(() => {
    setConfirmationTouched(true);
    clearMessages();

    if (!state.confirmation.trim()) {
      setState((prev) => ({
        ...prev,
        errorMessage: 'Please enter Y to confirm payment',
      }));
      return;
    }

    if (state.confirmation !== 'Y') {
      setState((prev) => ({
        ...prev,
        errorMessage: 'Invalid confirmation value. Please enter Y to confirm',
      }));
    }
  }, [state.confirmation, clearMessages]);

  const handleProcessPayment = useCallback(async (e: React.FormEvent) => {
    e.preventDefault();
    clearMessages();

    setAccountIdTouched(true);
    setConfirmationTouched(true);

    const trimmedAccountId = state.accountId.trim();

    if (!trimmedAccountId) {
      setState((prev) => ({
        ...prev,
        errorMessage: 'Acct ID can NOT be empty...',
      }));
      return;
    }

    if (!state.isAccountValid) {
      setState((prev) => ({
        ...prev,
        errorMessage: 'Please enter a valid account ID',
      }));
      return;
    }

    if (!state.hasBalance) {
      setState((prev) => ({
        ...prev,
        errorMessage: 'You have nothing to pay...',
      }));
      return;
    }

    if (!state.confirmation.trim()) {
      setState((prev) => ({
        ...prev,
        errorMessage: 'Please enter Y to confirm payment',
      }));
      return;
    }

    if (state.confirmation !== 'Y') {
      setState((prev) => ({
        ...prev,
        errorMessage: 'Invalid confirmation value. Please enter Y to confirm',
      }));
      return;
    }

    try {
      setState((prev) => ({ ...prev, isProcessing: true }));

      const result = await billPaymentService.processPayment({
        acctId: trimmedAccountId,
      });

      if (result.status === 'SUCCESS') {
        setState((prev) => ({
          ...prev,
          successMessage: `Payment processed successfully! Amount: $${result.paymentAmount.toFixed(2)}`,
          transactionId: result.acctId,
          currentBalance: result.newBalance,
          confirmation: '',
          isProcessing: false,
        }));
      } else {
        setState((prev) => ({
          ...prev,
          errorMessage: result.message || 'Payment processing failed',
          isProcessing: false,
        }));
      }
    } catch (error: any) {
      setState((prev) => ({
        ...prev,
        errorMessage: error.message || 'Failed to process payment',
        isProcessing: false,
      }));
    }
  }, [state.accountId, state.isAccountValid, state.hasBalance, state.confirmation, clearMessages]);

  const handleClear = useCallback(() => {
    setState({
      accountId: '',
      currentBalance: null,
      confirmation: '',
      isAccountValid: false,
      hasBalance: false,
      isProcessing: false,
      errorMessage: null,
      successMessage: null,
      transactionId: null,
    });
    setAccountIdTouched(false);
    setConfirmationTouched(false);
  }, []);

  const handleBack = useCallback(() => {
    router.push('/');
  }, [router]);

  const getCurrentDate = () => {
    const now = new Date();
    return now.toLocaleDateString('en-US', {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
    });
  };

  const getCurrentTime = () => {
    const now = new Date();
    return now.toLocaleTimeString('en-US', {
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
      hour12: false,
    });
  };

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-4xl mx-auto">
        <div className="bg-white shadow-lg rounded-lg overflow-hidden">
          <div className="bg-blue-600 text-white px-6 py-4">
            <div className="flex justify-between items-center">
              <h1 className="text-2xl font-bold">Bill Payment</h1>
              <div className="text-sm">
                <div>Date: {getCurrentDate()}</div>
                <div>Time: {getCurrentTime()}</div>
              </div>
            </div>
            <div className="text-sm mt-2">
              <div>Program: COBIL0A</div>
              {state.transactionId && <div>Transaction ID: {state.transactionId}</div>}
            </div>
          </div>

          <div className="p-6">
            {state.errorMessage && (
              <div className="mb-6 p-4 bg-red-50 border border-red-200 rounded-lg">
                <p className="text-red-800 font-semibold">{state.errorMessage}</p>
              </div>
            )}

            {state.successMessage && (
              <div className="mb-6 p-4 bg-green-50 border border-green-200 rounded-lg">
                <p className="text-green-800 font-semibold">{state.successMessage}</p>
              </div>
            )}

            <form onSubmit={handleProcessPayment} className="space-y-6">
              <div className="bg-gray-50 p-6 rounded-lg">
                <h2 className="text-lg font-semibold mb-4 text-gray-800">Account Information</h2>
                
                <div className="space-y-4">
                  <Input
                    label="Account ID"
                    value={state.accountId}
                    onChange={handleAccountIdChange}
                    onBlur={handleAccountIdBlur}
                    placeholder="Enter Account ID"
                    required
                    maxLength={11}
                    disabled={state.isProcessing}
                    error={accountIdTouched && !state.isAccountValid && state.accountId.trim() !== '' ? 'Invalid account ID' : undefined}
                  />

                  {state.currentBalance !== null && state.hasBalance && (
                    <div className="mt-4 p-4 bg-blue-50 border border-blue-200 rounded-lg">
                      <label className="block text-sm font-semibold text-gray-700 mb-1">
                        Current Balance
                      </label>
                      <p className="text-2xl font-bold text-blue-800">
                        ${state.currentBalance.toFixed(2)}
                      </p>
                    </div>
                  )}
                </div>
              </div>

              {state.isAccountValid && state.hasBalance && (
                <div className="bg-gray-50 p-6 rounded-lg">
                  <h2 className="text-lg font-semibold mb-4 text-gray-800">Payment Confirmation</h2>
                  
                  <div className="space-y-4">
                    <div className="p-4 bg-yellow-50 border border-yellow-200 rounded-lg">
                      <p className="text-sm text-yellow-800">
                        <strong>Note:</strong> This will process a payment for the full account balance of ${state.currentBalance?.toFixed(2)}
                      </p>
                    </div>

                    <Input
                      label="Confirm Payment (Enter Y to confirm)"
                      value={state.confirmation}
                      onChange={handleConfirmationChange}
                      onBlur={handleConfirmationBlur}
                      placeholder="Y"
                      required
                      maxLength={1}
                      disabled={state.isProcessing}
                      error={confirmationTouched && state.confirmation !== '' && state.confirmation !== 'Y' ? 'Must enter Y to confirm' : undefined}
                    />
                  </div>
                </div>
              )}

              <div className="flex gap-4 pt-4">
                <Button
                  type="submit"
                  disabled={state.isProcessing || !state.isAccountValid || !state.hasBalance || state.confirmation !== 'Y'}
                >
                  {state.isProcessing ? 'Processing...' : 'Process Payment'}
                </Button>
                
                <Button
                  type="button"
                  variant="secondary"
                  onClick={handleClear}
                  disabled={state.isProcessing}
                >
                  Clear
                </Button>
                
                <Button
                  type="button"
                  variant="secondary"
                  onClick={handleBack}
                  disabled={state.isProcessing}
                >
                  Back to Main Menu
                </Button>
              </div>
            </form>
          </div>

          <div className="bg-gray-100 px-6 py-3 text-sm text-gray-600">
            <p>
              <strong>Instructions:</strong> Enter your Account ID and press Tab or click outside the field to validate. 
              If the account has a balance, enter Y in the confirmation field and click Process Payment.
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}
